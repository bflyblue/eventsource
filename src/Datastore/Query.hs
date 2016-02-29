{-# LANGUAGE OverloadedStrings     #-}

module Datastore.Query where

import           Data.Text
import           Haxl.Core
import           People.Person
import           Datastore.Request

-- Haxl data store queries
-- This are the basic requests you should use to build up complex queries.

type Haxl = GenHaxl ()

getAllPeople :: Haxl [Person]
getAllPeople = dataFetch GetAllPeople

getPersonById :: Int -> Haxl (Maybe Person)
getPersonById id_ = dataFetch (GetPerson id_)

getPeopleByName :: Text -> Haxl [Person]
getPeopleByName name = dataFetch (GetPeopleByName name)
