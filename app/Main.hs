{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import           Datastore.Commands.TrainingProgram   as C
import           Datastore.Queries.TrainingProgram    as Q
import           Eventstore.PostgreSQL
import           Eventstore.PostgreSQL.CQRS
import           Eventstore.PostgreSQL.Internal.Store (runPgStore')
import           Eventstore.PostgreSQL.Internal.EventStream
import           Eventstore.PostgreSQL.Internal.Watch

import           Control.Concurrent

main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    conn  <- connect conninfo
    conn2 <- connect conninfo

    let tp = C.TrainingProgram "Program 1" [
                  C.Participant "Participant 1" 100
                , C.Participant "Participant 2" 120
                ]

    tp1 <- runCommand conn $
        createTrainingProgram tp

    _ <- forkIO $ do
        threadDelay 5000000

        p3 <- runCommand conn $
            addParticipant tp1 (C.Participant "Participant 3" 60)

        person <- runPgStore conn $ do
            snapshot p3
            rehydrate' p3

        print person

    print "waiting"

    let w = streamId tp1
    events <- runPgStore' conn2 $ do
        v <- getStream w
        watch [w]
        _ <- wait
        v' <- getStream w
        getEventsRange w v v'

    print events

    program <- runQuery conn $
        getTrainingProgram tp1

    runPgStore conn $ snapshot tp1

    print program
