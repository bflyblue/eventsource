{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Query where

import           Control.Concurrent.Async
import           Data.Hashable
import           Data.Pool
import           Data.Text
import           Data.Typeable
import           Database.PostgreSQL.Simple
import           Haxl.Core
import           Opaleye

import           People.Person

-- All the possible requests we can make of our Store parameterized by the return type.
data StoreRequest a where
    GetAllPeople        :: StoreRequest [Person]
    GetPeopleByName     :: Text -> StoreRequest [Person]
  deriving Typeable

deriving instance Show (StoreRequest a)
deriving instance Eq (StoreRequest a)
instance Show1 StoreRequest where show1 = show

-- We have to manually derive Hashable for a GADT unfortunately.
instance Hashable (StoreRequest a) where
    hashWithSalt s  GetAllPeople          = hashWithSalt s (0::Int)
    hashWithSalt s (GetPeopleByName name) = hashWithSalt s (1::Int, name)

instance DataSourceName StoreRequest where
    dataSourceName _ = "Store"

-- State for store requests.
-- We store the connection pool so we can re-use connections across multiple requests.
instance StateKey StoreRequest where
    data State StoreRequest =
        StoreState
          { dbpool    :: Pool Connection
          }

initStoreState :: Pool Connection -> State StoreRequest
initStoreState pool = StoreState { dbpool = pool }

-- Our fetch function for Haxl to handle Store Requests.
instance DataSource u StoreRequest where
    fetch = storeFetch

-- Handle blocked requests for our store.
-- Currently we dispatch the requests asyncronously using connections from our
-- connection pool. The amount of parallelism is therefore controlled by the
-- pool size.
-- TODO: This is where batching multiple requests into bulk requests shall happen.
storeFetch :: State StoreRequest -> Flags -> u -> [BlockedFetch StoreRequest] -> PerformFetch
storeFetch (StoreState pool) _flags _user requests = AsyncFetch go
  where
    go inner = do
        asyncs <- mapM fetchAsync requests  -- Fire off our fetches in parallel
        inner                               -- Perform the inner action
        mapM_ wait asyncs                   -- Make sure all our fetches are complete

    -- Perform a single request asyncronously using a connection from the pool.
    fetchAsync (BlockedFetch req rvar) = async $ do
        a <- withResource pool $ \conn -> fetchRequest conn req
        putSuccess rvar a                   -- TODO: handle failture

-- Request handler.
-- Use Opaleye to perform data fetches from our store.
fetchRequest :: Connection -> StoreRequest a -> IO a
fetchRequest conn  GetAllPeople          = runQuery conn personQuery
fetchRequest conn (GetPeopleByName name) = runQuery conn (personByName name)

type Haxl = GenHaxl ()


-- Haxl data store functions.
-- This are the basic requests you should use to build up complex queries.

getAllPeople :: Haxl [Person]
getAllPeople = dataFetch GetAllPeople

getPeopleByName :: Text -> Haxl [Person]
getPeopleByName name = dataFetch (GetPeopleByName name)
