{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Aggregate where

import Data.List (foldl')

type Version = Int

class Aggregate a where
    data EventT a

    empty   :: a
    apply   :: EventT a -> a -> a
    version :: a -> Version

data Tracked a = Tracked { currentState   :: a
                         , initialState   :: a
                         , trackedChanges :: [EventT a]
                         }

class EventStream a where
    type AggregateT a

    fetchEvents :: a -> Version -> Version -> IO [(Version, EventT (AggregateT a))]

rehydrateVersion :: (EventStream s, Aggregate (AggregateT s)) => Version -> s -> IO (Tracked (AggregateT s))
rehydrateVersion ver s = do
    events <- fetchEvents s 0 ver
    let a = foldl' (flip apply) empty (snd <$> events)
    return $ Tracked a empty (snd <$> events)

rehydrate :: (EventStream s, Aggregate (AggregateT s)) => s -> IO (Tracked (AggregateT s))
rehydrate = rehydrateVersion maxBound
