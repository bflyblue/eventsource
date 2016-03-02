{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module People.Person
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
import qualified Control.Category           as Cat
import           Data.Aeson
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.String
import           Data.Text                  (Text)
import           Opaleye

import           GHC.Generics               (Generic)

data Person' a b = Person
  { personId   :: a
  , personName :: b
  } deriving (Show, Eq, Generic)

type Person       = Person' Int Text
type PersonColumn = Person' (Column PGInt4) (Column PGText)

instance FromJSON Person
instance ToJSON Person

$(makeAdaptorAndInstance "pPerson" ''Person')

peopleTable :: Table (Person' (Maybe (Column PGInt4)) (Column PGText)) PersonColumn
peopleTable =
    Table "people"
        (pPerson Person { personId   = optional "id"
                        , personName = required "name"
                        })

personQuery :: Query PersonColumn
personQuery = queryTable peopleTable

data StrCmp = StrEq Text
    deriving (Show, Eq)

instance IsString StrCmp where
    fromString = StrEq . fromString

data PersonFilter = PersonId   Int
                  | PersonName StrCmp

restrictPerson :: PersonFilter -> QueryArr PersonColumn ()
restrictPerson (PersonId   id_         ) = proc p -> restrict -< (personId p   .== pgInt4 id_)
restrictPerson (PersonName (StrEq name)) = proc p -> restrict -< (personName p .== pgStrictText name)

-- TODO: I don't really grok Arrows yet so I'm sure there must be a cleaner way to do this...
filterPerson :: [PersonFilter] -> QueryArr PersonColumn ()
filterPerson [] = proc p -> returnA -< ()
filterPerson (f:fs) = proc p -> do
    restrictPerson f -< p
    filterPerson fs -< p

people :: [PersonFilter] -> Query PersonColumn
people fs = proc () -> do
    p <- personQuery -< ()
    filterPerson fs -< p
    returnA -< p
