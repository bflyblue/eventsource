{-# LANGUAGE FlexibleContexts #-}

module Datastore.Persist where

import Database.PostgreSQL.Simple
import Opaleye

import Aggregate
import Datastore.EventStream

persist :: Aggregate (Versioned a) => Connection -> Tracked (Versioned a) -> IO ()
persist conn (Tracked a _changes) = do
    _ <- runInsert conn eventStreamTable (EventStream Nothing (pgStrictText $ aggregateType a))
    return ()
