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
getAllPeople = dataFetch (GetPeople [])

getPersonById :: Int -> Haxl (Maybe Person)
getPersonById id_ = expectOne <$> dataFetch (GetPeople [PersonId id_])

getPeopleByName :: Text -> Haxl [Person]
getPeopleByName name = dataFetch (GetPeople [PersonName (StrEq name)])

expectOne :: [a] -> Maybe a
expectOne (x:_) = Just x
expectOne _ = Nothing
