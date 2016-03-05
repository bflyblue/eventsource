{-# LANGUAGE OverloadedStrings #-}

module Datastore.Store where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.PostgreSQL.Simple (Connection)
import EventSource.Store

data StoreEnv = StoreEnv { pgconn :: Connection }
data StoreErr = StoreInternalError String
              | StoreNotFound      String
              | StoreParseError    String
              | StoreTagMismatch
              deriving (Show, Eq, Ord)

type Store = StoreT StoreEnv StoreErr IO

withPgConn :: (Connection -> IO a) -> Store a
withPgConn a = do
    conn <- asks pgconn
    liftIO $ a conn

runStore :: Connection -> Store a -> IO (Either StoreErr a)
runStore conn = runStoreT env
    where env = StoreEnv { pgconn = conn }
