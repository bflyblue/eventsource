{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Datastore.Aggregates.Person where

import           EventStore.Aggregate
import           EventStore.PostgreSQL
import           EventStore.Version

import           Data.Aeson
import           Data.Hashable
import           Data.Text
import           GHC.Generics

data Person = Person
  { personName :: Text
  , personAge  :: Int
  } deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

type PersonId = StreamId (Versioned Person)

instance Aggregate (Versioned Person) where
    data EventT (Versioned Person)
      = SetPerson Text Int
      | ChangedName Text
      | ChangedAge Int
        deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

    empty = Initial
    apply (SetPerson   name age) = vset (Person name age)
    apply (ChangedName name    ) = vadjust (\p -> Update $ p { personName = name })
    apply (ChangedAge  age     ) = vadjust (\p -> Update $ p { personAge = age })

newPerson :: PgStore PersonId
newPerson = StreamId <$> newStream "person"

initPerson :: Person -> PgStore PersonId
initPerson (Person name age) = do
    p <- newPerson
    applyEvents p [SetPerson name age]
    return p
