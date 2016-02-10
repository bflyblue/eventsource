{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Data.Text
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Opaleye
import           People.Person

personByName :: Text -> Query PersonColumn
personByName name = proc () -> do
    p <- personQuery -< ()
    restrict -< (personName p .== pgStrictText name)
    returnA -< p

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='people'"
    ps <- runQuery conn (personByName "Joe Soap")
    print (ps :: [Person])
    joeid <- runInsert conn peopleTable Person { personId = Nothing, personName = pgStrictText "Joe Soap" }
    print joeid
