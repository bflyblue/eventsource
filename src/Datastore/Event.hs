{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Datastore.Event where

import           Datastore.Store
import           Datastore.Types
import           EventSource.Store

import           Control.Arrow
import           Data.Aeson
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Clock
import           GHC.Generics               (Generic)
import           Opaleye

data Event' a b c d = Event
  { eventId        :: a
  , eventStreamId  :: b
  , eventTimestamp :: c
  , eventPayload   :: d
  } deriving (Show, Eq, Generic)

type Event       = Event' Int Int UTCTime Value
type EventColumn = Event' (Column PGInt4) (Column PGInt4) (Column PGTimestamptz) (Column PGJsonb)

instance FromJSON Event
instance ToJSON Event

$(makeAdaptorAndInstance "pEvent" ''Event')

eventTable :: Table (Event' (Maybe (Column PGInt4))
                            (Column PGInt4)
                            (Maybe (Column PGTimestamptz))
                            (Column PGJsonb))
                    EventColumn
eventTable =
    Table "events"
        (pEvent Event { eventId         = optional "id"
                      , eventStreamId   = required "stream_id"
                      , eventTimestamp  = optional "timestamp"
                      , eventPayload    = required "payload"
                      })

eventQuery :: Query EventColumn
eventQuery = queryTable eventTable

newEvent :: ToJSON a => EventStreamId -> a -> Store EventId
newEvent (EventStreamId stream_id) event = do
    let e = Event Nothing (pgInt4 stream_id) Nothing (pgValueJSONB event)
    rs <- withPgConn $ \conn ->
        runInsertReturning conn eventTable e eventId
    case rs of
        [event_id] -> return $ EventId event_id
        _          -> storeErr $ StoreInternalError "INSERT RETURNING did not return valid event id"

getEvent :: EventId -> Store Event
getEvent (EventId event_id) = do
    events <- withPgConn $ \conn ->
        runQuery conn $ proc () -> do
            e <- eventQuery -< ()
            restrict -< eventId e .== pgInt4 event_id
            returnA -< e
    case events of
        [event] -> return event
        _       -> storeErr $ StoreNotFound $ "Zero or more than 1 event returned for id " ++ show event_id

getEventsForStream :: EventStreamId -> Store [Event]
getEventsForStream (EventStreamId steam_id) =
    withPgConn $ \conn ->
        runQuery conn $ proc () -> do
            e <- eventQuery -< ()
            restrict -< eventStreamId e .== pgInt4 steam_id
            returnA -< e

parseEventsForStream :: FromJSON a => EventStreamId -> Store [a]
parseEventsForStream stream_id = do
    events <- getEventsForStream stream_id
    let vals = eventPayload <$> events
    case mapM fromJSON vals of
        Success as -> return as
        Error msg  -> storeErr (StoreParseError msg)
