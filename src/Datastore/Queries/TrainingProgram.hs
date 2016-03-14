module Datastore.Queries.TrainingProgram where

import           Data.HashSet                         as Set
import           Data.Text

import           EventStore.PostgreSQL
import qualified Datastore.Aggregates.Participant     as A
import qualified Datastore.Aggregates.TrainingProgram as A
import           Datastore.CQRS.Query.Internal

data Participant = Participant
    { pName             :: Text
    } deriving Show

data TrainingProgram = TrainingProgram
    { tpName            :: Text
    , tpParticipants    :: [Participant]
    } deriving Show

getTrainingProgram :: A.TrainingProgramId -> Query TrainingProgram
getTrainingProgram tpid = do
    tp <- query $ rehydrate' tpid
    ps <- mapM getParticipant (Set.toList $ A.tpParticipants tp)
    return TrainingProgram
        { tpName            = A.tpName tp
        , tpParticipants    = ps
        }

getParticipant :: A.ParticipantId -> Query Participant
getParticipant pid = do
    p <- query $ rehydrate' pid
    return Participant { pName = A.pName p }
