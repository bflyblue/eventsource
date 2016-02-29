{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Pool
import           Database.PostgreSQL.Simple (connectPostgreSQL, close)
import           Haxl.Core (initEnv, stateSet, stateEmpty, runHaxl)

import           Query

main :: IO ()
main = do
    pool <- createPool (connectPostgreSQL "dbname='people'")
                       close
                       1
                       5
                       8
    let peopleState = initPeopleState pool
    env <- initEnv (stateSet peopleState stateEmpty) ()
    r <- runHaxl env $ getPeopleByName "Joe Soap"

    print r

    -- joeid <- runInsert conn peopleTable Person { personId = Nothing, personName = pgStrictText "Joe Soap" }
    -- print joeid
