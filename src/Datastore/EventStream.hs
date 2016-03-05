{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Datastore.EventStream where

import           Datastore.Event
import           Datastore.Store
import           Datastore.Types
import           EventSource.Store          (storeErr)

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Opaleye

data EventStream' a b c = EventStream
  { streamId   :: a
  , streamType :: b
  , streamTag  :: c
  } deriving (Show, Eq, Generic)

type EventStream = EventStream' Int Text Int
type EventStreamColumn = EventStream' (Column PGInt4) (Column PGText) (Column PGInt4)

instance FromJSON EventStream
instance ToJSON EventStream

$(makeAdaptorAndInstance "pEventStream" ''EventStream')

eventStreamTable :: Table (EventStream' (Maybe (Column PGInt4))
                                        (Column PGText)
                                        (Column PGInt4))
                          EventStreamColumn
eventStreamTable =
    Table "event_streams"
        (pEventStream EventStream { streamId   = optional "id"
                                  , streamType = required "type"
                                  , streamTag  = required "tag"
                                  })

eventStreamQuery :: Query EventStreamColumn
eventStreamQuery = queryTable eventStreamTable

newEventStream :: Text -> Store EventStreamId
newEventStream t = do
    let es = EventStream Nothing (pgStrictText t) (pgInt4 0)
    rs <- withPgConn $ \conn ->
        runInsertReturning conn eventStreamTable es streamId
    case rs of
        [stream_id] -> return $ EventStreamId stream_id
        _           -> storeErr $ StoreInternalError "INSERT RETURNING did not return valid event stream id"

getEventStream :: EventStreamId -> Store EventStream
getEventStream (EventStreamId stream_id) = do
    streams <- withPgConn $ \conn ->
        runQuery conn $ proc () -> do
            s <- eventStreamQuery -< ()
            restrict -< streamId s .== pgInt4 stream_id
            returnA -< s
    case streams of
        [stream] -> return stream
        _        -> storeErr $ StoreNotFound $ "Zero or more than 1 event streams returned for id " ++ show stream_id

updateEventStreamTag :: EventStreamId -> Int -> Int -> Store Bool
updateEventStreamTag (EventStreamId stream_id) old new = do
    n <- withPgConn $ \conn ->
        runUpdate conn eventStreamTable setTag (\e -> streamId  e .== pgInt4 stream_id
                                                  .&& streamTag e .== pgInt4 old)
    return $ n == 1
  where
    setTag EventStream{..} = EventStream { streamId   = Nothing
                                         , streamType = streamType
                                         , streamTag  = pgInt4 new }

-- TODO: check updateEventSteamTag is sufficient for concurrency concerns
addEvents :: EventStreamId -> Int -> [Event] -> Store Bool
addEvents stream_id tag events = do
    updated <- updateEventStreamTag stream_id tag (tag + length events)
    when updated $
        mapM_ (newEvent stream_id) events
    return updated

-- TODO: try make this a single database query
fetchEvents :: EventStreamId -> Store (Int, [Event])
fetchEvents stream_id = do
    s <- getEventStream stream_id
    events <- getEventsForStream stream_id
    return (streamTag s, events)
