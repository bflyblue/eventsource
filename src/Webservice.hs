{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Webservice where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either   (EitherT, left)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Haxl.Core
import           Network.Wai.Handler.Warp
import           Servant

import           Api                          (API)
import           People.Person                (Person, Person' (..))
import           Query

type App = ReaderT AppState (EitherT ServantErr IO)

data AppState = AppState
  { appEnv     :: Env ()
  , appMetrics :: Maybe AppMetrics
  }

data AppMetrics = AppMetrics

serveForever :: Env () -> Port -> Maybe AppMetrics -> IO ()
serveForever env port metrics = do
    let state = AppState env metrics
    runResourceT $ runNoLoggingT $ liftIO $ run port (app state)
  where
    serverWithState s = enter (runReaderTNat s) server
    app = serve (Proxy :: Proxy API) . serverWithState

appError :: ServantErr -> App a
appError = lift . left

server :: ServerT API App
server = getPeople

haxl :: Haxl a -> App a
haxl a= do
    env <- asks appEnv
    liftIO $ runHaxl env a

getPeople :: App [Person]
getPeople = haxl getAllPeople
