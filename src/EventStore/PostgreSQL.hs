{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module EventStore.PostgreSQL
( PgStore
, PgStoreError(..)
, StreamId(..)
, runPgStore
, rehydrate
, applyEvents
, snapshot
, newStream
) where


import           EventStore.Aggregate                       as A
import           EventStore.PostgreSQL.Internal.Cache
import           EventStore.PostgreSQL.Internal.Delta
import           EventStore.PostgreSQL.Internal.EventStream
import           EventStore.PostgreSQL.Internal.Snapshot
import           EventStore.PostgreSQL.Internal.Store
import           EventStore.PostgreSQL.Internal.Types

import           Data.Aeson
import           Data.Hashable                              (Hashable)
import           Data.Typeable                              (Typeable)

rehydrate :: (Typeable a, Aggregate a, FromJSON a, FromJSON (EventT a)) => StreamId a -> PgStore a
rehydrate stream = do
    maggr <- cacheLookup stream
    case maggr of
        Just a  -> return a
        Nothing -> do
            (ver, snap) <- getStreamSnap (streamId stream)
            (a, es) <- case snap of
                Just snapver -> do
                    s <- getSnapshot (streamId stream) snapver
                    jsonEvents <- getEventsRange (streamId stream) snapver ver
                    s' <- fromResult (fromJSON s)
                    return (s', jsonEvents)
                Nothing -> do
                    jsonEvents <- getEvents (streamId stream) ver
                    return (A.empty, jsonEvents)
            events <- fromResult (mapM fromJSON es)
            let a' = foldE a events
            cacheInsert stream a'
            deltaInit stream ver
            return a'
  where
    fromResult (Success r) = return r
    fromResult (Error msg) = throwError msg

snapshot :: (Typeable a, Aggregate a, FromJSON (EventT a), FromJSON a, ToJSON a) => StreamId a -> PgStore ()
snapshot stream = do
    a <- rehydrate stream
    Just (Delta ver _) <- deltaLookup stream
    snapshotStream (streamId stream) ver (toJSON a)

applyEvents :: (Typeable a, Eq (StreamId a), Hashable (StreamId a), ToJSON (EventT a), Aggregate a) => StreamId a -> [EventT a] -> PgStore ()
applyEvents stream events = do
    let jsonEvents = toJSON <$> events
    mdelta <- deltaLookup stream
    delta  <- case mdelta of
                Just (Delta ver es) -> return $ Delta ver (es ++ jsonEvents)
                Nothing             -> Delta <$> getStream (streamId stream) <*> pure jsonEvents
    deltaInsert stream delta

    maggr <- cacheLookup stream
    case maggr of
        Just a  -> cacheInsert stream (foldE a events)
        Nothing -> return ()
