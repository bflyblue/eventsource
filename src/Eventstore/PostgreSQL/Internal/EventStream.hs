{-# LANGUAGE OverloadedStrings #-}

module Eventstore.PostgreSQL.Internal.EventStream where

import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson
import           Database.PostgreSQL.Simple

import           Eventstore.PostgreSQL.Internal.Types
import           Eventstore.PostgreSQL.Internal.Watch

newStream :: String -> PgStore Int
newStream stype = do
    conn <- getConn
    streams <- liftIO $ query conn "insert into event_streams (type, version) values (?,?) returning id"
                                   (stype, 0 :: Int)
    case streams of
        [Only stream] -> return stream
        _             -> throwError "INSERT RETURNING didn't return stream id"

getStream :: Int -> PgStore Version
getStream stream = do
    conn <- getConn
    vers <- liftIO $ query conn "select version from event_streams where id = ?"
                                (Only stream)
    case vers of
        [Only ver] -> return ver
        _          -> throwError "Event stream not found"

updateStream :: Int -> Version -> Version -> PgStore ()
updateStream stream old new = do
    conn <- getConn
    nrows <- liftIO $ execute conn "update event_streams set version = ? where id = ? and version = ?"
                                   (new, stream, old)
    when (nrows == 0) $ throwError "Update Conflict"
    notify stream

getEvents :: Int -> Version -> PgStore [Value]
getEvents stream version = do
    conn <- getConn
    vals <- liftIO $ query conn "select payload from events where stream_id = ? and index <= ? order by index asc"
                                (stream, version)
    return $ fromOnly <$> vals

getEventsRange :: Int -> Version -> Version -> PgStore [Value]
getEventsRange stream after till = do
    conn <- getConn
    vals <- liftIO $ query conn "select payload from events where stream_id = ? and index > ? and index <= ? order by index asc"
                                (stream, after, till)
    return $ fromOnly <$> vals

addEvents :: Int -> Version -> [Value] -> PgStore ()
addEvents stream version events = do
    conn <- getConn
    _nrows <- liftIO $ executeMany conn "insert into events (stream_id, index, payload) values (?, ?, ?)"
                                        [(stream, version + i, e) | (i, e) <- zip [1..] events]
    return ()
