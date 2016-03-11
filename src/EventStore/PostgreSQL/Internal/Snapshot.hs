{-# LANGUAGE OverloadedStrings #-}

module EventStore.PostgreSQL.Internal.Snapshot where

import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson
import           Database.PostgreSQL.Simple

import           EventStore.PostgreSQL.Internal.Types

snapshotStream :: Int -> Version -> Value -> PgStore ()
snapshotStream stream version value = do
    conn <- getConn
    _ <- liftIO $ execute conn "insert into snapshots (stream_id, version, payload) values (?,?,?) on conflict (stream_id, version) do update set payload = ?"
                               (stream, version, value, value)
    _ <- liftIO $ execute conn "update event_streams set snapshot = ? where id = ? and coalesce(snapshot, 0) < ?"
                               (version, stream, version)
    return ()

getStreamSnap :: Int -> PgStore (Version, Maybe Version)
getStreamSnap stream = do
    conn <- getConn
    vers <- liftIO $ query conn "select version, snapshot from event_streams where id = ?"
                                (Only stream)
    case vers of
        [verpair] -> return verpair
        _         -> throwError "Event stream not found"

getSnapshot :: Int -> Version -> PgStore Value
getSnapshot stream version = do
    conn <- getConn
    vals <- liftIO $ query conn "select payload from snapshots where stream_id = ? and version = ?"
                                (stream, version)
    case vals of
        [Only val] -> return val
        _          -> throwError "Snapshot not found"
