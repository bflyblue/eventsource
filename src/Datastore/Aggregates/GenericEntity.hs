{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Datastore.Aggregates.GenericEntity where

import           Eventstore.Aggregate
import           Eventstore.PostgreSQL
import           Eventstore.Version

import           Control.Monad
import           Data.Aeson
import           Data.HashMap.Strict   as Map
import           Data.Text             (Text)

import           GHC.Generics

type AttributeList = [(Text, GenericValue)]
type Attributes = HashMap Text GenericValue

data GenericValue
    = GInt    Integer
    | GStr    Text
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data GenericEntity = GenericEntity
  { geAttributes :: Attributes
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

type GenericEntityId = StreamId (Versioned GenericEntity)

instance Aggregate (Versioned GenericEntity) where
    data EventT (Versioned GenericEntity)
      = CreatedGenericEntity AttributeList
      | UpdatedAttributes AttributeList
        deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

    empty = Initial

    apply (CreatedGenericEntity attrs) =
        vset (GenericEntity $ Map.fromList attrs)

    apply (UpdatedAttributes attrs) =
        vadjust (\e -> Update $ e { geAttributes = geAttributes e `Map.union` Map.fromList attrs })

newGenericEntity :: PgStore GenericEntityId
newGenericEntity = StreamId <$> newStream "GenericEntity"

createGenericEntity :: Schema -> AttributeList -> PgStore GenericEntityId
createGenericEntity schema attrs = do
    unless (validateAttrs schema attrs) (throwError "Invalid attributes")
    e <- newGenericEntity
    applyEvents e [CreatedGenericEntity attrs]
    return e

updateAttributes :: Schema -> GenericEntityId -> AttributeList -> PgStore ()
updateAttributes schema e attrs = do
    unless (validateAttrs schema attrs) (throwError "Invalid attributes")
    applyEvents e [UpdatedAttributes attrs]


-- Validation

type Schema     = HashMap Text AttribSpec
data AttribSpec = TInt (Integer -> Bool)
                | TStr (Text    -> Bool)

mkSchema :: [(Text, AttribSpec)] -> Schema
mkSchema = Map.fromList

validateEntity :: Schema -> GenericEntity -> Bool
validateEntity schema (GenericEntity attrs) = validateAttrs schema (Map.toList attrs)

validateAttrs :: Schema -> AttributeList -> Bool
validateAttrs schema = all validateAttrib
  where
    validateAttrib (attrname, attrval) =
        case Map.lookup attrname schema of
            Nothing   -> False
            Just spec -> validate spec attrval

    validate (TInt c) (GInt i) = c i
    validate (TInt _) _        = False
    validate (TStr c) (GStr t) = c t
    validate (TStr _) _        = False
