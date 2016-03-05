{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple

import Datastore.Store
import Datastore.Aggregates.Person

import EventSource.EventStream as ES


main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    conn <- connect conninfo

    r <- runStore conn $ do
        shaun <- newPerson
        (tag, p) <- ES.rehydrate shaun
        r1 <- ES.addEvents shaun tag [SetPerson "Shaun" 39]
        es <- ES.fetchEvents shaun
        (tag2, p2) <- ES.rehydrate shaun
        return (p, r1, es, tag2, p2)

    print r
