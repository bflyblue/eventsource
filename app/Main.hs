{-# LANGUAGE OverloadedStrings #-}

module Main where

import Datastore.Aggregates.TrainingProgram
import Database.PostgreSQL.Simple
import EventStore.PostgreSQL
import EventStore.Version

import qualified Data.HashSet as Set

main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    conn <- connect conninfo

    tp1 <- runPgStore conn $ do
        tp  <- initTrainingProgram "Program 1"
        _p1 <- addParticipant tp "Participant 1"
        _p2 <- addParticipant tp "Participant 2"
        return tp

    runPgStore conn $ do
        _p3 <- addParticipant tp1 "Participant 3"
        _p4 <- addParticipant tp1 "Participant 4"
        return ()

    program <- runPgStore conn $ do
        snapshot tp1
        prog  <- rehydrate tp1
        parts <- case prog of
                    (Version _ p) -> mapM rehydrate (Set.toList $ tpParticipants p)
                    _             -> return []
        return (prog, parts)

    print program
