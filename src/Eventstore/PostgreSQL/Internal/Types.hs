{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eventstore.PostgreSQL.Internal.Types where

import           Control.Exception              (Exception, throw)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.RWS.Strict (RWST, ask)
import           Data.Aeson
import           Data.Functor.Identity          (Identity)
import           Data.Hashable                  (Hashable)
import           Data.Map.Strict                as Map
import           Database.PostgreSQL.Simple     (Connection)
import           GHC.Generics
import           Haxl.Core.DataCache            as DataCache

newtype PgStore a = PgStore
    { unPgStore :: RWST Connection () PgState IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

data PgState = PgState
    { sCache  :: DataCache Identity
    , sDeltas :: Map Int Delta
    }

data PgStoreError = InternalError String deriving (Show, Eq, Ord)
instance Exception PgStoreError

data Delta = Delta Version (Maybe Version) [Value] deriving (Show, Eq)

newtype StreamId a = StreamId { streamId :: Int } deriving (Show, Eq, Ord, Generic, Hashable)
instance FromJSON (StreamId a)
instance ToJSON (StreamId a)

type Version = Int


-- TODO: find a better home for these
emptyPgState :: PgState
emptyPgState = PgState DataCache.empty Map.empty

throwError :: String -> PgStore a
throwError = PgStore . liftIO . throw . InternalError

getConn :: PgStore Connection
getConn = PgStore ask
