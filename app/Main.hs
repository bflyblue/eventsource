{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import qualified Datastore.Aggregates.TrainingProgram as A
import           Datastore.Commands.TrainingProgram
import           EventStore.PostgreSQL

import qualified Data.HashSet                         as Set

main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    conn <- connect conninfo

    let tp = TrainingProgram "Program 1" [
                  Participant "Participant 1"
                , Participant "Participant 2"
                ]

    tp1 <- runPgStore conn $
        createTrainingProgram tp

    program <- runPgStore conn $ do
        snapshot tp1
        prog  <- rehydrate' tp1
        parts <- mapM rehydrate' (Set.toList $ A.tpParticipants prog)
        return (prog, parts)

    print program
