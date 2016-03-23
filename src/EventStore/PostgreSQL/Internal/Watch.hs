{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Eventstore.PostgreSQL.Internal.Watch where

import           Control.Monad
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Trans.RWS.Strict          (ask, get, gets, put)
import           Data.ByteString.Char8                   (readInt)
import           Data.ByteString.Lazy                    as L
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Data.Set                                as Set
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Notification
import           Database.PostgreSQL.Simple.SqlQQ

import           Eventstore.PostgreSQL.Internal.Types

watch :: [Int] -> PgStore ()
watch streams = PgStore $ do
    conn <- ask
    s <- get
    when (Set.null (sWatches s)) $ liftIO $ void $ execute_ conn [sql|listen event_stream_updated|]
    put s { sWatches = Set.union (Set.fromList streams) (sWatches s) }

unwatch :: [Int] -> PgStore ()
unwatch streams = PgStore $ do
    conn <- ask
    s <- get
    let w' = Set.difference (Set.fromList streams) (sWatches s)
    when (Set.null w') $ liftIO $ void $ execute_ conn [sql|unlisten event_stream_updated|]
    put s { sWatches = w' }

wait :: PgStore Int
wait = do
    (conn, watches) <- PgStore $ (,) <$> ask <*> gets sWatches
    when (Set.null watches) $ throwError "Watch set is empty"
    nextNotification conn watches
  where
    nextNotification conn watches = do
        n <- liftIO $ getNotification conn
        if notificationChannel n == "event_stream_updated"
        then
            case readInt (notificationData n) of
                Just (streamid, "") -> if Set.member streamid watches
                                       then return streamid
                                       else nextNotification conn watches
                _                   -> nextNotification conn watches
        else
            nextNotification conn watches

notify :: Int -> PgStore ()
notify stream = PgStore $ do
    conn <- ask
    let payload = toSmall $ intDec stream
    liftIO $ void $ execute conn [sql|notify event_stream_updated, ?|] (Only payload)
  where
    toSmall = toStrict . toSmallLazy
    toSmallLazy = toLazyByteStringWith (untrimmedStrategy 32 128) L.empty
