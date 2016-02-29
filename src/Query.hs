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

data StoreRequest a where
    GetPeopleByName     :: Text -> StoreRequest [Person]
  deriving Typeable

deriving instance Show (StoreRequest a)
deriving instance Eq (StoreRequest a)
instance Show1 StoreRequest where show1 = show

instance Hashable (StoreRequest a) where
    hashWithSalt s (GetPeopleByName name) = hashWithSalt s (0::Int, name)

instance DataSourceName StoreRequest where
    dataSourceName _ = "Store"

instance StateKey StoreRequest where
    data State StoreRequest =
        StoreState
          { dbpool    :: Pool Connection
          }

initStoreState :: Pool Connection -> State StoreRequest
initStoreState pool = StoreState { dbpool = pool }

instance DataSource u StoreRequest where
    fetch = peopleFetch

peopleFetch :: State StoreRequest -> Flags -> u -> [BlockedFetch StoreRequest] -> PerformFetch
peopleFetch (StoreState pool) _flags _user requests = AsyncFetch go
  where
    go inner = do
        asyncs <- mapM fetchAsync requests
        inner
        mapM_ wait asyncs

    fetchAsync (BlockedFetch req rvar) = async $ do
        a <- withResource pool $ \conn -> fetchRequest conn req
        putSuccess rvar a

fetchRequest :: Connection -> StoreRequest a -> IO a
fetchRequest conn (GetPeopleByName name) = runQuery conn (personByName name)

type Haxl = GenHaxl ()

getPeopleByName :: Text -> GenHaxl u [Person]
getPeopleByName name = dataFetch (GetPeopleByName name)
