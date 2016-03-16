{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Eventstore.PostgreSQL.Internal.CommandQueue where

import           Control.Monad
import           Control.Monad.IO.Class                  (liftIO)
import           Data.Aeson
import           Data.Text
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Notification
import           Database.PostgreSQL.Simple.SqlQQ

import           Eventstore.PostgreSQL.Internal.Types

queueCommand :: Value -> PgStore Int
queueCommand cmd = do
    conn <- getConn
    qcmds <- liftIO $ query conn [sql|insert into command_queue (payload) values (?) returning id|]
                                 (Only cmd)
    case qcmds of
        [Only qcmd] -> return qcmd
        _           -> throwError "INSERT RETURNING didn't return command queue id"

withCommand :: (Value -> PgStore (Either Value Value)) -> PgStore ()
withCommand a = do
    (qid, cmd) <- awaitCommand
    e <- a cmd
    case e of
        Left  r -> failCommand qid r
        Right r -> completeCommand qid r

awaitCommand :: PgStore (Int, Value)
awaitCommand = do
    conn <- getConn
    liftIO $ do
        listen conn
        cmd <- waitCommand conn
        unlisten conn
        return cmd

  where
    listen conn = void $ execute_ conn [sql|listen command_queue_changed|]
    unlisten conn = void $ execute_ conn [sql|unlisten command_queue_changed|]

    waitCommand conn = do
        c <- getCommand conn
        case c of
            Just cmd -> return cmd
            Nothing  -> do
                waitNotification conn
                waitCommand conn

    waitNotification conn = do
        n <- getNotification conn
        print $ notificationChannel n
        unless (notificationChannel n == "command_queue_changed") $ waitNotification conn

    getCommand conn = do
        cmds <- query conn [sql|update command_queue set status = ?, started = now() where id = (
                                  select id from command_queue where status = ?
                                  order by "timestamp" asc limit 1
                                ) returning id, payload|]
                           (busy, waiting)
        case cmds of
            [(qid, cmd)] -> return $ Just (qid, cmd)
            _            -> return Nothing

updateCommand :: Text -> Text -> Int -> Value -> PgStore ()
updateCommand old new qid result = do
    conn <- getConn
    nrows <- liftIO $ execute conn [sql|update command_queue set status = ?, result = ?, completed = now()
                                        where id = ? and status = ?|]
                                   (new, result, qid, old)
    when (nrows == 0) $ throwError "Queue item in unexpected state"

completeCommand :: Int -> Value -> PgStore ()
completeCommand = updateCommand busy done

failCommand :: Int -> Value -> PgStore ()
failCommand = updateCommand busy failed

waiting, busy, done, failed :: Text
waiting = "waiting"
busy    = "busy"
done    = "done"
failed  = "failed"
