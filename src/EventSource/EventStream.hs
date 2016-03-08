{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module EventSource.EventStream where

import EventSource.Aggregate

class Aggregate (AggregateT es) => EventStream es where
    type AggregateT es
    type StoreT es :: * -> *

    addEvents        :: es -> [EventT (AggregateT es)] -> StoreT es Bool
    fetchEvents      :: es -> StoreT es [EventT (AggregateT es)]

rehydrate :: (EventStream es, Monad (StoreT es)) => es -> StoreT es (AggregateT es)
rehydrate stream = foldE <$> fetchEvents stream
