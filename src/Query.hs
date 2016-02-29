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

data PeopleRequest a where
    GetPeopleByName     :: Text -> PeopleRequest [Person]
  deriving Typeable

deriving instance Show (PeopleRequest a)
deriving instance Eq (PeopleRequest a)
instance Show1 PeopleRequest where show1 = show

instance Hashable (PeopleRequest a) where
    hashWithSalt s (GetPeopleByName name) = hashWithSalt s (0::Int, name)

instance DataSourceName PeopleRequest where
    dataSourceName _ = "People Store"

instance StateKey PeopleRequest where
    data State PeopleRequest =
        PeopleState
          { dbpool    :: Pool Connection
          }

initPeopleState :: Pool Connection -> State PeopleRequest
initPeopleState pool = PeopleState { dbpool = pool }

instance DataSource u PeopleRequest where
    fetch = peopleFetch

peopleFetch :: State PeopleRequest -> Flags -> u -> [BlockedFetch PeopleRequest] -> PerformFetch
peopleFetch (PeopleState pool) _flags _user requests = AsyncFetch go
  where
    go inner = do
        asyncs <- mapM fetchAsync requests
        inner
        mapM_ wait asyncs

    fetchAsync (BlockedFetch req rvar) = async $ do
        a <- withResource pool $ \conn -> fetchRequest conn req
        putSuccess rvar a

fetchRequest :: Connection -> PeopleRequest a -> IO a
fetchRequest conn (GetPeopleByName name) = runQuery conn (personByName name)

type Haxl = GenHaxl ()

getPeopleByName :: Text -> GenHaxl u [Person]
getPeopleByName name = dataFetch (GetPeopleByName name)
