{-# LANGUAGE TypeFamilies #-}

module Aggregate where

import           Data.List   (foldl')

data Versioned a = Initial | Version Int a | Invalid Int | Deleted Int
    deriving (Show, Eq, Ord)

version :: Versioned a -> Int
version Initial       = 0
version (Version v _) = v
version (Invalid v)   = v
version (Deleted v)   = v

vadjust :: (a -> a) -> Versioned a -> Versioned a
vadjust f (Version v a) = Version (succ v) (f a)
vadjust _ u             = Invalid (succ (version u))

vset :: a -> Versioned a -> Versioned a
vset a Initial       = Version 0        a
vset a (Version v _) = Version (succ v) a
vset a (Deleted v)   = Version (succ v) a   -- TODO: does this make sense?
vset a (Invalid v)   = Version (succ v) a   -- TODO: does this make sense?

class Aggregate a where
    data AggregateEvent a

    empty :: a
    apply :: AggregateEvent a -> a -> a

    rehydrate :: [AggregateEvent a] ->  a
    rehydrate = foldl' (@>) empty

(@>) :: Aggregate a => a -> AggregateEvent a -> a
(@>) = flip apply

data Tracked a = Tracked { currentState :: a, changes :: [AggregateEvent a] }

-- track :: Aggregate a => AggregateEvent a -> Tracked a -> Tracked a
-- track e (Tracked a es) = Tracked (apply e a) (es <> [e])
