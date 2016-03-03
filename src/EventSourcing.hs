{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module EventSourcing where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.List                  (foldl')
import           Database.PostgreSQL.Simple

data StoreEnv = StoreEnv { pgconn :: Connection }
data StoreErr = StoreErr String

type Store = ReaderT StoreEnv (EitherT StoreErr IO)

runStore :: StoreEnv -> Store a -> IO (Either StoreErr a)
runStore env = runEitherT . flip runReaderT env

storeErr :: StoreErr -> Store a
storeErr = lift . left

type Ver = Int

class Aggregate a where
    data EventT a
    version :: a -> Ver
    empty   :: a
    apply   :: EventT a -> a -> a
    foldE   :: (Aggregate a, Traversable t) => t (EventT a) -> a

    foldE = foldl' (flip apply) empty

class Aggregate (AggregateT a) => EventStream a where
    data AggregateT a
    appendEvents     :: a -> [EventT (AggregateT a)] -> Store ()
    fetchEvents      :: a -> Store [EventT (AggregateT a)]
    fetchEventsRange :: Ver -> Ver -> a -> Store [EventT (AggregateT a)]

    fetchEvents = fetchEventsRange 0 maxBound

    fetchEventsRange first last_ a | last_ >= first = take (last_ - first) . drop first <$> fetchEvents a
                                   | otherwise      = storeErr (StoreErr "Invalid range")

rehydrate :: EventStream a => a -> Store (AggregateT a)
rehydrate stream = do
    events <- fetchEvents stream
    return $! foldE events
