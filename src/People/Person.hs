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
)
where

import Data.Aeson
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Opaleye

import GHC.Generics (Generic)

data Person' a b = Person
  { personId        :: a
  , personName      :: b
  } deriving (Eq, Show, Generic)

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
