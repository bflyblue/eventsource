{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Datastore.Aggregates.Participant where

import           Eventstore.Aggregate
import           Eventstore.PostgreSQL
import           Eventstore.Version

import           Data.Aeson
import           Data.Hashable
import           Data.Text
import           GHC.Generics

data Participant = Participant
  { pName   :: Text
  , pSpend  :: Integer
  } deriving (Show, Eq, Ord, Generic, Hashable)

instance FromJSON Participant
instance ToJSON Participant

type ParticipantId = StreamId (Versioned Participant)

instance Aggregate (Versioned Participant) where
    data EventT (Versioned Participant)
      = CreatedParticipant Text Integer
      | ChangedParticipantName Text
        deriving (Show, Eq, Ord, Generic)

    empty = Initial
    apply (CreatedParticipant name spend) = vset (Participant name spend)
    apply (ChangedParticipantName name)   = vadjust (\p -> Update $ p { pName = name })

instance FromJSON (EventT (Versioned Participant))
instance ToJSON (EventT (Versioned Participant))

newParticipant :: PgStore ParticipantId
newParticipant = StreamId <$> newStream "Participant"

initParticipant :: Text -> Integer -> PgStore ParticipantId
initParticipant name spend = do
    p <- newParticipant
    applyEvents p [CreatedParticipant name spend]
    return p
