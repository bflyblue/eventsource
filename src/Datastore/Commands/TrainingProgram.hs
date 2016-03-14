module Datastore.Commands.TrainingProgram where

import           Data.Text

import           EventStore.PostgreSQL
import qualified Datastore.Aggregates.Participant     as A
import qualified Datastore.Aggregates.TrainingProgram as A

data Participant = Participant
    { pName             :: Text
    } deriving Show

data TrainingProgram = TrainingProgram
    { tpName            :: Text
    , tpParticipants    :: [Participant]
    } deriving Show

createTrainingPrograms :: [TrainingProgram] -> PgStore [A.TrainingProgramId]
createTrainingPrograms = mapM createTrainingProgram

createTrainingProgram :: TrainingProgram -> PgStore A.TrainingProgramId
createTrainingProgram (TrainingProgram name participants) = do
    tpid <- A.initTrainingProgram name
    mapM_ (addParticipant tpid) participants
    return tpid

addParticipant :: A.TrainingProgramId -> Participant -> PgStore A.ParticipantId
addParticipant tp (Participant name) =
    A.addParticipant tp name
