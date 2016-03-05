{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Person.Person where

{-
module Person.Person
( Person'(..)
, Person
, PersonColumn
, peopleTable
, personQuery
, people
, PersonFilter(..)
, StrCmp(..)
) where

import           Control.Arrow
import           Data.Aeson
import           Data.Hashable
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.String
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Opaleye

import           EventSourcing
import           Version

data Person' a b = Person
  { personId      :: a
  , personName    :: b
  } deriving (Show, Eq, Generic)

type Person       = Person' Int Text
type PersonColumn = Person' (Column PGInt4) (Column PGText)

instance FromJSON Person
instance ToJSON Person

$(makeAdaptorAndInstance "pPerson" ''Person')

instance Aggregate (Versioned Person) where
    data EventT (Versioned Person)
      = InitialisePerson Int Text
      | UpdatePersonName Text
      deriving (Show, Eq, Ord, Generic)

    version = Version.version

    empty = Initial

    apply (InitialisePerson id_ name)               = vset    Person { personId = id_, personName = name }
    apply (UpdatePersonName name    )               = vadjust (\p -> p { personName = name })

instance FromJSON (EventT (Versioned Person))
instance ToJSON   (EventT (Versioned Person))

peopleTable :: Table (Person' (Maybe (Column PGInt4)) (Column PGText)) PersonColumn
peopleTable =
    Table "people"
        (pPerson Person { personId      = optional "id"
                        , personName    = required "name"
                        })

personQuery :: Query PersonColumn
personQuery = queryTable peopleTable

-- TODO: This should move to it's own module
data StrCmp = StrEq Text
    deriving (Show, Eq, Generic, Hashable)

instance IsString StrCmp where
    fromString = StrEq . fromString

-- Different ways of restricting a people query
data PersonFilter = PersonId   Int
                  | PersonName StrCmp
    deriving (Show, Eq, Generic, Hashable)

-- Restrict query based on filters
restrictPerson :: PersonFilter -> QueryArr PersonColumn ()
restrictPerson (PersonId   id_         ) = proc p -> restrict -< (personId p   .== pgInt4 id_)
restrictPerson (PersonName (StrEq name)) = proc p -> restrict -< (personName p .== pgStrictText name)

-- TODO: I don't really grok Arrows yet so I'm sure there must be a cleaner way to do this...
filterPerson :: [PersonFilter] -> QueryArr PersonColumn ()
filterPerson [] = proc _ -> returnA -< ()
filterPerson (f:fs) = proc p -> do
    restrictPerson f -< p
    filterPerson fs -< p

people :: [PersonFilter] -> Query PersonColumn
people fs = proc () -> do
    p <- personQuery -< ()
    filterPerson fs -< p
    returnA -< p
-}
