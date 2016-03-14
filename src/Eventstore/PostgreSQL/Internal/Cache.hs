{-# LANGUAGE FlexibleContexts #-}

module Eventstore.PostgreSQL.Internal.Cache where

import           Control.Monad.Trans.RWS.Strict       (get, gets, put)
import           Data.Functor.Identity                (Identity (..), runIdentity)
import           Data.Typeable                        (Typeable)
import           Haxl.Core.DataCache                  as DataCache

import           Eventstore.PostgreSQL.Internal.Types

cacheLookup :: Typeable a => StreamId a -> PgStore (Maybe a)
cacheLookup stream = PgStore $ do
    cache <- gets sCache
    return $ runIdentity <$> DataCache.lookup stream cache

cacheInsert :: Typeable a => StreamId a -> a -> PgStore ()
cacheInsert stream a = PgStore $ do
    s <- get
    put s { sCache = DataCache.insertNotShowable stream (Identity a) (sCache s) }
