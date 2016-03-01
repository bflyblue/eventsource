{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Webservice.Server where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either   (EitherT, left)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Pool
import           Database.PostgreSQL.Simple   (Connection)
import           Haxl.Core                    hiding (env)
import           Network.Wai.Handler.Warp
import           Servant

import           Datastore.Query
import           Datastore.Request
import           People.Person                (Person)
import           Webservice.Api               (API)

-- Our custom App type
-- We use ReaderT to hold some useful data
type App = ReaderT AppEnv (EitherT ServantErr IO)

data AppEnv = AppEnv
  { appHaxlEnv :: Env ()
  , appMetrics :: Maybe AppMetrics
  }

data AppMetrics = AppMetrics

-- Serve our API forever on a TCP port
serveForever :: Pool Connection -> Port -> Maybe AppMetrics -> IO ()
serveForever pool port metrics =
    runResourceT $ runNoLoggingT $ liftIO $ run port app
  where
    serverWithState s = enter (runReaderTNat s) server
    app req respond = do
        -- Create a fresh Haxl environment to handle our requests with a blank cache.
        let storeState = initStoreState pool
        env <- initEnv (stateSet storeState stateEmpty) ()
        let state = AppEnv env metrics
        serve (Proxy :: Proxy API) (serverWithState state) req respond

-- Run a Haxl query
haxl :: Haxl a -> App a
haxl a = do
    env <- asks appHaxlEnv
    liftIO $ runHaxl env a

-- Lift servant errors into our App type
appError :: ServantErr -> App a
appError = lift . left

-- Server definition conforms to the API
-- This is where we associate functions to endpoints in our api
server :: ServerT API App
server = getPeople
    :<|> getPerson

getPeople :: App [Person]
getPeople = haxl getAllPeople

getPerson :: Int -> App Person
getPerson id_ = do
    mperson <- haxl (getPersonById id_)
    case mperson of
        Just person -> return person
        Nothing     -> appError err404
