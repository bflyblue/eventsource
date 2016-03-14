{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Datastore.CQRS.Query.Internal
( Query(..)
, query
, runQuery
)
where

import Database.PostgreSQL.Simple (Connection)
import EventStore.PostgreSQL

newtype Query a = Query (PgStore a) deriving (Functor, Applicative, Monad)

query :: PgStore a -> Query a
query = Query

runQuery :: Connection -> Query a -> IO a
runQuery conn (Query action) = runPgStore conn action
