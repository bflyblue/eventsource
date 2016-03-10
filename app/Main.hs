{-# LANGUAGE OverloadedStrings #-}

module Main where

import EventStore.PostgreSQL.Store
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
        s <- initPerson (Person "Shaun" 39)
        applyEvents s [ChangedName "Shaun Sharples", ChangedAge 21]
        applyEvents s [ChangedAge 39]
        return s

    print shaun

    person <- runPgStore conn $
        rehydrate shaun

    print person
