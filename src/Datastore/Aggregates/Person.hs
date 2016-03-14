{-# LANGUAGE OverloadedStrings #-}

module Datastore.Aggregates.Person where

import           Datastore.Aggregates.GenericEntity
import           Eventstore.PostgreSQL

import           Data.Text as Text

personSchema :: Schema
personSchema = mkSchema
    [ ("Name",  TStr cNotEmptyStr)
    , ("Age",   TInt cPosInt)
    ]
  where
    cNotEmptyStr = not . Text.null
    cPosInt      = (> 0)

createPerson :: AttributeList -> PgStore GenericEntityId
createPerson = createGenericEntity personSchema

createPerson' :: Text -> Integer -> PgStore GenericEntityId
createPerson' name age = createPerson [("Name", GStr name), ("Age", GInt age)]
