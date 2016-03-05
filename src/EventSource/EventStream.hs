{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module EventSource.EventStream where

import EventSource.Aggregate

class Aggregate (AggregateT s) => EventStream s where
    type AggregateT s
    type StreamTag s
    type StreamStore s :: * -> *
    addEvents        :: s -> StreamTag s -> [EventT (AggregateT s)] -> StreamStore s Bool
    fetchEvents      :: s -> StreamStore s (StreamTag s, [EventT (AggregateT s)])

rehydrate :: (EventStream s, Monad (StreamStore s)) => s -> StreamStore s (StreamTag s, AggregateT s)
rehydrate stream = do
    (tag, events) <- fetchEvents stream
    return (tag, foldE events)
