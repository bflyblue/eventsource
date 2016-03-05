{-# LANGUAGE TypeFamilies #-}

module EventSource.Aggregate where

import           Data.List (foldl')

class Aggregate a where
    data EventT a
    version :: a -> Int
    empty :: a
    apply :: EventT a -> a -> a
    foldE :: (Aggregate a, Traversable t) => t (EventT a) -> a
    foldE = foldl' (flip apply) empty
