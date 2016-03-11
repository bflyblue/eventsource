{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EventStore.PostgreSQL.Internal.Types where

import           Control.Exception              (Exception, throw)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.RWS.Strict (RWST, ask)
import           Data.Aeson                     (Value)
import           Data.Functor.Identity          (Identity)
import           Data.Hashable                  (Hashable)
import           Data.Map.Strict                as Map
import           Database.PostgreSQL.Simple     (Connection)
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

data Delta = Delta Version [Value] deriving (Show, Eq)

newtype StreamId a = StreamId { streamId :: Int } deriving (Show, Eq, Ord, Hashable)

type Version = Int


-- TODO: find a better home for these
emptyPgState :: PgState
emptyPgState = PgState DataCache.empty Map.empty

throwError :: String -> PgStore a
throwError = PgStore . liftIO . throw . InternalError

getConn :: PgStore Connection
getConn = PgStore ask
