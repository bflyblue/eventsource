{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import qualified Datastore.Aggregates.TrainingProgram as A
import qualified Datastore.Aggregates.Person          as A
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

    p1 <- runPgStore conn $
        A.createPerson' "Shaun" 39

    person <- runPgStore conn $ do
        snapshot p1
        rehydrate' p1

    print person

    p2 <- runPgStore conn $
        A.createPerson' "Shaun" (-5)

    person2 <- runPgStore conn $ do
        snapshot p2
        rehydrate' p2

    print person2
