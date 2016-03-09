{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Datastore.Aggregates.Person where

import qualified EventStore.Aggregate   as ES
import           EventStore.PostgreSQL.Store

import GHC.Generics
import Data.Aeson
import Data.Hashable
import Data.Text

data Person = Person
  { personName :: Text
  , personAge  :: Int
  } deriving (Show, Eq, Ord, Generic, Hashable)

instance FromJSON Person
instance ToJSON Person

type PersonId = StreamId Person

instance ES.Aggregate Person where
    data EventT Person
      = SetPerson Text Int
      | ChangePersonAge Int
        deriving (Show, Eq, Ord, Generic)

    empty = Person "" 0
    apply _               (SetPerson name age)  = Person name age
    apply (Person name _) (ChangePersonAge age) = Person name age

instance FromJSON (ES.EventT Person)
instance ToJSON (ES.EventT Person)

newPerson :: PgStore PersonId
newPerson = StreamId <$> newStream "person"
