module Datastore.Types where

newtype EventStreamId = EventStreamId Int deriving (Show, Eq)
newtype EventId = EventId Int deriving (Show, Eq)
