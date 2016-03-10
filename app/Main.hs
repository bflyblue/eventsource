{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Pool
import Datastore.Aggregates.Person
import Database.PostgreSQL.Simple
import EventStore.PostgreSQL.Store


main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    pool <- createPool (connect conninfo) close 1 5 20

    shaun <- withResource pool $ flip runPgStore (initPerson (Person "Shaun" 39))

    asyncs <- replicateM 20 $ async (updateThread pool shaun)
    mapM_ wait asyncs

updateThread :: Pool Connection -> PersonId -> IO ()
updateThread pool person = replicateM_ 1000 $ do
    go
    threadDelay 1

  where
    go =
        withResource pool $ \conn -> do
            p <- runPgStore conn $ rehydrate person
            print p

            u <- try $ runPgStore conn $ do
                applyEvents person [ChangedName "Shaun Sharples", ChangedAge 21]
                applyEvents person [ChangedAge 39]
            case u of
                Left (InternalError msg) -> do
                    putStrLn $ "Update failed: " ++ msg ++ ", retrying..."
                    go
                Right () -> putStrLn "Update successful"
