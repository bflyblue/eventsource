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

module Datastore.Command where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics               (Generic)
import           Opaleye

import           Aggregate

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

eventTable :: Table (Event' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGTimestamptz) (Column PGJsonb)) EventColumn
eventTable =
    Table "events"
        (pEvent Event { eventId         = optional "id"
                      , eventStreamId   = required "stream_id"
                      , eventTimestamp  = required "timestamp"
                      , eventPayload    = required "payload"
                      })

eventQuery :: Query EventColumn
eventQuery = queryTable eventTable

type Command = IO

rehydrate :: (Aggregate a, FromJSON (AggregateEvent a))
          => Connection -> Int -> IO (Result a)
rehydrate conn stream = do
    events <- runQuery conn $ proc () -> do
        e <- eventQuery -< ()
        restrict -< eventStreamId e .== pgInt4 stream
        returnA -< e
    return $ foldM applyEvent empty events

applyEvent :: (Aggregate a, FromJSON (AggregateEvent a))
           => a -> Event -> Result a
applyEvent a e = apply <$> fromJSON (eventPayload e) <*> pure a

createPerson :: Text -> Command Int
createPerson = undefined
