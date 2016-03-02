{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Datastore.EventStream where

import           Data.Aeson
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Opaleye

data EventStream' a b = EventStream
  { eventStreamId   :: a
  , eventStreamType :: b
  } deriving (Show, Eq, Generic)

type EventStream = EventStream' Int Text
type EventStreamColumn = EventStream' (Column PGInt4) (Column PGText)

instance FromJSON EventStream
instance ToJSON EventStream

$(makeAdaptorAndInstance "pEventStream" ''EventStream')

eventStreamTable :: Table (EventStream' (Maybe (Column PGInt4)) (Column PGText)) EventStreamColumn
eventStreamTable =
    Table "event_streams"
        (pEventStream EventStream { eventStreamId   = optional "id"
                                  , eventStreamType = required "type"
                                  })

eventStreamQuery :: Query EventStreamColumn
eventStreamQuery = queryTable eventStreamTable
