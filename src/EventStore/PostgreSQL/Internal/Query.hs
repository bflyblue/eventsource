{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eventstore.PostgreSQL.Internal.Query
( Query(..)
, query
, runQuery
)
where

import Database.PostgreSQL.Simple (Connection)
import Eventstore.PostgreSQL.Internal.Store
import Eventstore.PostgreSQL.Internal.Types

newtype Query a = Query (PgStore a) deriving (Functor, Applicative, Monad)

query :: PgStore a -> Query a
query = Query

runQuery :: Connection -> Query a -> IO a
runQuery conn (Query action) = runPgStore conn action
