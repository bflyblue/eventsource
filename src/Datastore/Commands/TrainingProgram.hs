module Datastore.Commands.TrainingProgram where

import           Data.Text

import qualified Datastore.Aggregates.Participant     as A
import qualified Datastore.Aggregates.TrainingProgram as A
import           Datastore.CQRS.Command.Internal

data Participant = Participant
    { pName             :: Text
    } deriving Show

data TrainingProgram = TrainingProgram
    { tpName            :: Text
    , tpParticipants    :: [Participant]
    } deriving Show

createTrainingPrograms :: [TrainingProgram] -> Command [A.TrainingProgramId]
createTrainingPrograms = mapM createTrainingProgram

createTrainingProgram :: TrainingProgram -> Command A.TrainingProgramId
createTrainingProgram (TrainingProgram name participants) = do
    tpid <- command $ A.initTrainingProgram name
    mapM_ (addParticipant tpid) participants
    return tpid

addParticipant :: A.TrainingProgramId -> Participant -> Command A.ParticipantId
addParticipant tp (Participant name) =
    command $ A.addParticipant tp name
