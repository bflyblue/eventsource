{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Pool
import           Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo, connect, close)
import           Haxl.Core (initEnv, stateSet, stateEmpty, runHaxl)

import           Query

main :: IO ()
main = do
    -- TODO: Get connection and pool settings from the environment or as
    --       command-line arguments.

    -- Data store connection information.
    -- This is the PostgreSQL database we're using as a data store.
    -- example:  { connectDatabase = "store"
    --           , connectHost     = "localhost"
    --           , connectPort     = 5432
    --           , connectUser     = "postgres"
    --           , connectPass     = "password"
    --           }
    let conninfo = defaultConnectInfo { connectDatabase = "people" }

    -- Use a connection pool rather than opening a new connection for every request.
    pool <- createPool (connect conninfo) close
                       1    -- stripes
                       5    -- amount of time to keep idle connection open
                       8    -- maximum connections per stripe

    -- Create a Haxl environment to handle our data store requests
    let storeState = initStoreState pool
    env <- initEnv (stateSet storeState stateEmpty) ()

    -- Example request
    r <- runHaxl env $ getPeopleByName "Joe Soap"
    print r

    -- joeid <- runInsert conn peopleTable Person { personId = Nothing, personName = pgStrictText "Joe Soap" }
    -- print joeid

    destroyAllResources pool
