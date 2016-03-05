{-# LANGUAGE OverloadedStrings #-}

module Datastore.Store where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import EventSource.Store

data StoreEnv = StoreEnv { pgconn :: Connection }
data StoreErr = StoreInternalError String
              | StoreNotFound      String
              deriving (Show, Eq, Ord)

type Store = StoreT StoreEnv StoreErr IO

withPgConn :: (Connection -> IO a) -> Store a
withPgConn a = do
    conn <- asks pgconn
    liftIO $ a conn

runStore :: Connection -> Store a -> IO (Either StoreErr a)
runStore conn = runStoreT env
    where env = StoreEnv { pgconn = conn }

neptune :: IO Connection
neptune = connectPostgreSQL "dbname=store host=neptune password=icecream"