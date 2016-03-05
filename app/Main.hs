{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import Datastore.Store
import Datastore.Event
import Datastore.EventStream

main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    conn <- connect conninfo

    r <- runStore conn $ do
        a  <- newEventStream "typeA"
        e1 <- newEvent a ([1,2,3] :: [Int])
        e2 <- newEvent a ([4,5,6] :: [Int])
        return (a, e1, e2)

    print r
