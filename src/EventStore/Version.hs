module EventStore.Version where

data Versioned a = Initial | Version Int a | Invalid Int | Deleted Int
    deriving (Show, Eq, Ord)

data Change a = Update a | Delete | Invalidate
    deriving (Show, Eq, Ord)

version :: Versioned a -> Int
version Initial       = 0
version (Version v _) = v
version (Invalid v)   = v
version (Deleted v)   = v

vadjust :: (a -> Change a) -> Versioned a -> Versioned a
vadjust f (Version v a) = case f a of
                            Update a'  -> Version (succ v) a'
                            Delete     -> Deleted (succ v)
                            Invalidate -> Invalid (succ v)
vadjust _ u             = Invalid (succ (version u))

vset :: a -> Versioned a -> Versioned a
vset a Initial       = Version 1        a
vset a (Version v _) = Version (succ v) a
vset _ (Deleted v)   = Invalid (succ v)
vset _ (Invalid v)   = Invalid (succ v)

vreset :: a -> Versioned a -> Versioned a
vreset a Initial       = Version 1        a
vreset a (Version v _) = Version (succ v) a
vreset a (Deleted v)   = Version (succ v) a
vreset a (Invalid v)   = Version (succ v) a
