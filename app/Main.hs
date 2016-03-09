{-# LANGUAGE OverloadedStrings #-}

module Main where

import EventSource.PostgreSQL.Store
import Datastore.Aggregates.Person
import Database.PostgreSQL.Simple


main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    conn <- connect conninfo

    shaun <- runPgStore conn $ do
        p <- newPerson
        s <- eventStream p
        _ <- applyEvents s [SetPerson "Shaun" 39, ChangePersonAge 21]
        _ <- applyEvents s [ChangePersonAge 24]
        return p

    print shaun

    person <- runPgStore conn $ do
        s <- eventStream shaun
        rehydrate s

    print person
