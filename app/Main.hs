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
        s <- newPerson
        applyEvents s [SetPerson "Shaun" 39, ChangePersonAge 21]
        applyEvents s [ChangePersonAge 24]
        return s

    print shaun

    person <- runPgStore conn $
        rehydrate shaun

    print person
