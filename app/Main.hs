{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import qualified Datastore.Aggregates.Person          as A
import           Datastore.Commands.TrainingProgram   as C
import           Datastore.Queries.TrainingProgram    as Q
import           Eventstore.PostgreSQL
import           Eventstore.PostgreSQL.CQRS

main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    conn <- connect conninfo

    let tp = C.TrainingProgram "Program 1" [
                  C.Participant "Participant 1"
                , C.Participant "Participant 2"
                ]

    tp1 <- runCommand conn $
        createTrainingProgram tp

    runPgStore conn $ snapshot tp1

    program <- runQuery conn $
        getTrainingProgram tp1

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
