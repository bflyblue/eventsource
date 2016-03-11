{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Datastore.Aggregates.TrainingProgram where

import           EventStore.Aggregate
import           EventStore.PostgreSQL
import           EventStore.Version

import           Control.Monad
import           Data.Aeson
import           Data.HashSet                     as Set
import           Data.Text
import           GHC.Generics

import           Datastore.Aggregates.Participant

data TrainingProgram = TrainingProgram
  { tpName              :: Text
  , tpParticipants      :: HashSet ParticipantId
  , tpUniqueNames       :: HashSet Text
  } deriving (Show, Eq, Generic)

instance FromJSON TrainingProgram
instance ToJSON TrainingProgram

type TrainingProgramId = StreamId (Versioned TrainingProgram)

instance Aggregate (Versioned TrainingProgram) where
    data EventT (Versioned TrainingProgram)
      = CreatedTrainingProgram Text
      | ChangedTrainingProgramName Text
      | AddedParticipant ParticipantId Text
        deriving (Show, Eq, Ord, Generic)

    empty = Initial

    apply (CreatedTrainingProgram name) =
        vset (TrainingProgram name Set.empty Set.empty)

    apply (ChangedTrainingProgramName name) =
        vadjust (\tp -> Update $ tp { tpName = name })

    apply (AddedParticipant participant name) =
        vadjust (\tp -> Update $ tp { tpParticipants = Set.insert participant (tpParticipants tp)
                                    , tpUniqueNames  = Set.insert name        (tpUniqueNames tp ) })

instance FromJSON (EventT (Versioned TrainingProgram))
instance ToJSON (EventT (Versioned TrainingProgram))
instance FromJSON (Versioned TrainingProgram)
instance ToJSON (Versioned TrainingProgram)

newTrainingProgram :: PgStore TrainingProgramId
newTrainingProgram = StreamId <$> newStream "TrainingProgram"

initTrainingProgram :: Text -> PgStore TrainingProgramId
initTrainingProgram name = do
    tp <- newTrainingProgram
    applyEvents tp [CreatedTrainingProgram name]
    return tp

addParticipant :: TrainingProgramId -> Text -> PgStore ParticipantId
addParticipant tp name = do
    vtp <- rehydrate tp
    case vtp of
        Version _ program -> when (name `Set.member` tpUniqueNames program) (throwError "Participant with name already exists")
        _                 -> throwError "Training Program in invalid state"
    p <- initParticipant name
    applyEvents tp [AddedParticipant p name]
    return p
