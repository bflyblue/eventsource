{-# LANGUAGE OverloadedStrings #-}

module Datastore.Commands.TrainingProgram where

import           Control.Monad.IO.Class                 (liftIO)
import           Data.HashSet                           as Set
import           Data.Text
import           Database.PostgreSQL.Simple

import qualified Datastore.Aggregates.Participant       as A
import qualified Datastore.Aggregates.TrainingProgram   as A
import           Eventstore.PostgreSQL
import           Eventstore.PostgreSQL.Internal.Command
import           Eventstore.PostgreSQL.Internal.Types

data Participant = Participant
    { pName  :: Text
    , pSpend :: Integer
    } deriving Show

data TrainingProgram = TrainingProgram
    { tpName         :: Text
    , tpParticipants :: [Participant]
    } deriving Show

createTrainingPrograms :: [TrainingProgram] -> Command [A.TrainingProgramId]
createTrainingPrograms = mapM createTrainingProgram

createTrainingProgram :: TrainingProgram -> Command A.TrainingProgramId
createTrainingProgram (TrainingProgram name participants) = do
    tpid <- command $ A.initTrainingProgram name
    mapM_ (addParticipant tpid) participants
    return tpid

addParticipant :: A.TrainingProgramId -> Participant -> Command A.ParticipantId
addParticipant tpid (Participant name spend) =
    command $ A.addParticipant tpid name spend

calculateSpend :: A.TrainingProgramId -> Command ()
calculateSpend tpid = command $ do
    tp <- rehydrate' tpid
    spends <- mapM getSpend (Set.toList $ A.tpParticipants tp)
    let total = sum spends

    conn <- getConn
    _ <- liftIO $ execute conn "insert into spends (training_program_id, total_spend) values (?,?) on conflict (training_program_id) do update set total_spend = ?"
                               (streamId tpid, total, total)
    return ()

  where
     getSpend pid = do
         p <- rehydrate' pid
         return $ A.pSpend p
