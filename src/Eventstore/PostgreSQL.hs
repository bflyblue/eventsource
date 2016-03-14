{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eventstore.PostgreSQL
( PgStore
, PgStoreError(..)
, StreamId(..)
, runPgStore
, throwError
, rehydrate
, rehydrate'
, applyEvents
, snapshot
, newStream
) where

import           Eventstore.Aggregate                       as A
import           Eventstore.Version
import           Eventstore.PostgreSQL.Internal.Cache
import           Eventstore.PostgreSQL.Internal.Delta
import           Eventstore.PostgreSQL.Internal.EventStream
import           Eventstore.PostgreSQL.Internal.Snapshot
import           Eventstore.PostgreSQL.Internal.Store
import           Eventstore.PostgreSQL.Internal.Types

import           Data.Aeson
import           Data.Typeable                              (Typeable)

rehydrate' :: (Typeable (Versioned a), Aggregate (Versioned a), FromJSON (Versioned a), FromJSON (EventT (Versioned a)))
          => StreamId (Versioned a) -> PgStore a
rehydrate' stream = do
    va <- rehydrate stream
    case va of
        Version _ a -> return a
        Initial     -> throwError "Attempt to use uninitialised entity"
        Deleted _   -> throwError "Attempt to use deleted entity"
        Invalid _   -> throwError "Attempt to use invalidated entity"

rehydrate :: (Typeable a, Aggregate a, FromJSON a, FromJSON (EventT a))
          => StreamId a -> PgStore a
rehydrate stream = do
    maggr <- cacheLookup stream
    case maggr of
        Just a  -> return a
        Nothing -> do
            mdelta <- deltaLookup stream
            case mdelta of
                Just (Delta ver snap es') -> do
                    (a, es) <- getAggrAndEvents ver snap
                    events <- fromResult (mapM fromJSON $ es ++ es')
                    let a' = foldE a events
                    cacheInsert stream a'
                    return a'
                Nothing -> do
                    (ver, snap) <- getStreamSnap (streamId stream)
                    (a, es) <- getAggrAndEvents ver snap
                    events <- fromResult (mapM fromJSON es)
                    let a' = foldE a events
                    cacheInsert stream a'
                    deltaInit stream ver snap
                    return a'
  where
    getAggrAndEvents ver (Just snapver) = do
        s <- getSnapshot (streamId stream) snapver
        jsonEvents <- getEventsRange (streamId stream) snapver ver
        s' <- fromResult (fromJSON s)
        return (s', jsonEvents)

    getAggrAndEvents ver Nothing = do
        jsonEvents <- getEvents (streamId stream) ver
        return (A.empty, jsonEvents)

    fromResult (Success r) = return r
    fromResult (Error msg) = throwError msg

snapshot :: (Typeable a, Aggregate a, FromJSON a, FromJSON (EventT a), ToJSON a)
         => StreamId a -> PgStore ()
snapshot stream = do
    a <- rehydrate stream
    Just (Delta ver _ _) <- deltaLookup stream
    snapshotStream (streamId stream) ver (toJSON a)

applyEvents :: (Typeable a, Aggregate a, ToJSON (EventT a))
            => StreamId a -> [EventT a] -> PgStore ()
applyEvents stream events = do
    let jsonEvents = toJSON <$> events
    mdelta <- deltaLookup stream
    delta  <- case mdelta of
                Just (Delta ver snap es) -> return $ Delta ver snap (es ++ jsonEvents)
                Nothing                  -> do  (ver, snap) <- getStreamSnap (streamId stream)
                                                return $ Delta ver snap jsonEvents
    deltaInsert stream delta

    maggr <- cacheLookup stream
    case maggr of
        Just a  -> cacheInsert stream (foldE a events)
        Nothing -> return ()
