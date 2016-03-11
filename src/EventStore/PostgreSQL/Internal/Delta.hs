
{-# LANGUAGE FlexibleContexts #-}

module EventStore.PostgreSQL.Internal.Delta where

import           Control.Monad.Trans.RWS.Strict       (get, put)
import qualified Data.Map.Strict                      as Map

import           EventStore.PostgreSQL.Internal.Types

deltaInit :: StreamId a -> Version -> Maybe Version -> PgStore ()
deltaInit stream version snapshot = deltaInsert stream (Delta version snapshot [])

deltaInsert :: StreamId a -> Delta -> PgStore ()
deltaInsert stream delta = PgStore $ do
    s <- get
    put s { sDeltas = Map.insert (streamId stream) delta (sDeltas s) }

deltaLookup :: StreamId a -> PgStore (Maybe Delta)
deltaLookup stream = PgStore $ do
    s <- get
    return $ Map.lookup (streamId stream) (sDeltas s)
