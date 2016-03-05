{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module EventSource.EventStream where

import EventSource.Aggregate
import EventSource.Store

import Control.Monad.IO.Class

class Aggregate (AggregateT s) => EventStream s where
    type AggregateT s
    type StreamTag s
    addEvents        :: MonadIO m => s -> StreamTag s -> [EventT (AggregateT s)] -> StoreT env err m Bool
    fetchEvents      :: MonadIO m => s -> StoreT env err m (StreamTag s, [EventT (AggregateT s)])

rehydrate :: (MonadIO m, EventStream s) => s -> StoreT env err m (StreamTag s, AggregateT s)
rehydrate stream = do
    (tag, events) <- fetchEvents stream
    return (tag, foldE events)
