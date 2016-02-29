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
import           People.Person                (Person, Person' (..))
import           Webservice.Api               (API)

type App = ReaderT AppState (EitherT ServantErr IO)

data AppState = AppState
  { appEnv     :: Env ()
  , appMetrics :: Maybe AppMetrics
  }

data AppMetrics = AppMetrics

serveForever :: Pool Connection -> Port -> Maybe AppMetrics -> IO ()
serveForever pool port metrics =
    runResourceT $ runNoLoggingT $ liftIO $ run port app
  where
    serverWithState s = enter (runReaderTNat s) server
    app req respond = do
        -- Create a fresh Haxl environment to handle our requests with a blank cache.
        let storeState = initStoreState pool
        env <- initEnv (stateSet storeState stateEmpty) ()
        let state = AppState env metrics
        serve (Proxy :: Proxy API) (serverWithState state) req respond

appError :: ServantErr -> App a
appError = lift . left

server :: ServerT API App
server = getPeople
    :<|> getPerson

haxl :: Haxl a -> App a
haxl a= do
    env <- asks appEnv
    liftIO $ runHaxl env a

getPeople :: App [Person]
getPeople = haxl getAllPeople

getPerson :: Int -> App Person
getPerson id_ = do
    mperson <- haxl (getPersonById id_)
    case mperson of
        Just person -> return person
        Nothing     -> appError err404
