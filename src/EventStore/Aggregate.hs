{-# LANGUAGE TypeFamilies #-}

module EventStore.Aggregate where

import Data.List (foldl')

class Aggregate a where
    data EventT a
    empty :: a
    apply :: a -> EventT a -> a

    foldE :: (Aggregate a, Foldable t) => a -> t (EventT a) -> a
    foldE = foldl' apply
