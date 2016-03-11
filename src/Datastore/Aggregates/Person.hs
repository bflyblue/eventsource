{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Datastore.Aggregates.Person where

import EventStore.Aggregate
import EventStore.Version
import EventStore.PostgreSQL

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

type PersonId = StreamId (Versioned Person)

instance Aggregate (Versioned Person) where
    data EventT (Versioned Person)
      = SetPerson Text Int
      | ChangedName Text
      | ChangedAge Int
        deriving (Show, Eq, Ord, Generic)

    empty = Initial
    apply (SetPerson   name age) = vset (Person name age)
    apply (ChangedName name    ) = vadjust (\p -> Update $ p { personName = name })
    apply (ChangedAge  age     ) = vadjust (\p -> Update $ p { personAge = age })

instance FromJSON (EventT (Versioned Person))
instance ToJSON (EventT (Versioned Person))
instance FromJSON (Versioned Person)
instance ToJSON (Versioned Person)

newPerson :: PgStore PersonId
newPerson = StreamId <$> newStream "person"

initPerson :: Person -> PgStore PersonId
initPerson (Person name age) = do
    p <- newPerson
    applyEvents p [SetPerson name age]
    return p
