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

import           Datastore.Store
import           EventSource.Store          (storeErr)

import           Data.Aeson
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Opaleye

newtype EventStreamId = EventStreamId Int deriving (Show, Eq)

data EventStream' a b c = EventStream
  { eventStreamId   :: a
  , eventStreamType :: b
  , eventStreamTag  :: c
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
        (pEventStream EventStream { eventStreamId   = optional "id"
                                  , eventStreamType = required "type"
                                  , eventStreamTag  = required "tag"
                                  })

eventStreamQuery :: Query EventStreamColumn
eventStreamQuery = queryTable eventStreamTable

newEventStream :: Text -> Store EventStreamId
newEventStream t = do
    let es = EventStream Nothing (pgStrictText t) (pgInt4 0)
    rs <- withPgConn $ \conn ->
        runInsertReturning conn eventStreamTable es eventStreamId
    case rs of
        [stream_id] -> return $ EventStreamId stream_id
        _           -> storeErr $ StoreInternalError "INSERT RETURNING did not return valid event stream id"

updateEventStreamTag :: EventStreamId -> Int -> Int -> Store Bool
updateEventStreamTag (EventStreamId stream_id) old new = do
    n <- withPgConn $ \conn ->
        runUpdate conn eventStreamTable setTag (\e -> eventStreamId  e .== pgInt4 stream_id
                                                  .&& eventStreamTag e .== pgInt4 old)
    return $ n == 1
  where
    setTag EventStream{..} = EventStream { eventStreamId   = Nothing
                                         , eventStreamType = eventStreamType
                                         , eventStreamTag  = pgInt4 new }

-- fetchEvents :: FromJSON a => EventStreamId -> Store [a]
-- fetchEvents = undefined
