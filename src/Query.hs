{-# LANGUAGE OverloadedStrings     #-}

module Query where

import           Data.Text
import           Haxl.Core
import           People.Person
import           Request

-- Haxl data store queries
-- This are the basic requests you should use to build up complex queries.

type Haxl = GenHaxl ()

getAllPeople :: Haxl [Person]
getAllPeople = dataFetch GetAllPeople

getPeopleByName :: Text -> Haxl [Person]
getPeopleByName name = dataFetch (GetPeopleByName name)
