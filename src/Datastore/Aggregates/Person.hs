{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Datastore.Aggregates.Person where

import Datastore.EventStream             as DS
import Datastore.Store
import Datastore.Types
import qualified EventSource.Aggregate   as ES
import qualified EventSource.EventStream as ES

import GHC.Generics
import Data.Aeson
import Data.Text

data Person = Person
  { personName :: Text
  , personAge  :: Int
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Person
instance ToJSON Person

newtype PersonId = PersonId EventStreamId

instance ES.Aggregate Person where
    data EventT Person
      = SetPerson Text Int
      | ChangePersonAge Int
        deriving (Show, Eq, Ord, Generic)

    version _ = 0
    empty = Person "" 0
    apply (SetPerson name age)  _               = Person name age
    apply (ChangePersonAge age) (Person name _) = Person name age

instance FromJSON (ES.EventT Person)
instance ToJSON (ES.EventT Person)

instance ES.EventStream PersonId where
    type AggregateT PersonId = Person
    type StreamTag PersonId = Int
    type StreamStore PersonId = Store

    addEvents (PersonId stream_id)   = DS.addEvents stream_id
    fetchEvents (PersonId stream_id) = DS.fetchEvents stream_id

newPerson :: Store PersonId
newPerson = PersonId <$> newEventStream "person"
