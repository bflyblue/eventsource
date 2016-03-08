{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module EventStore.PostgreSQL.Store where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.Strict
import Data.Aeson
import Data.Either
import Data.Hashable
import Data.Typeable
import Database.PostgreSQL.Simple
import Haxl.Core.DataCache as DataCache

import EventSource.Aggregate as A

newtype StreamId a = StreamId { streamId :: Int }
type Version = Int
type Cache = DataCache Versioned
data Versioned a = Versioned Version a [EventT a]
newtype SVal a = SVal (StreamId a)

newtype PgStore a = PgStore { unPgStore :: RWST Connection () Cache (ExceptT String IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

runPgStore :: Connection -> PgStore a -> IO (Either String a)
runPgStore conn a =
    -- uses PostgreSQL's per-connection 'default_transaction_isolation' variable which by
    -- default is ReadCommitted and sufficient for us:
    -- http://www.postgresql.org/docs/9.5/static/transaction-iso.html
    withTransaction conn $ do
        er <- runExceptT (fst <$> evalRWST (unPgStore a) conn DataCache.empty)
        when (isLeft er) $ rollback conn
        return er

throwStoreE :: String -> PgStore a
throwStoreE = PgStore . lift . throwE

getConn :: PgStore Connection
getConn = PgStore ask

cacheLookup :: Typeable a => StreamId a -> PgStore (Maybe (Versioned a))
cacheLookup stream = PgStore $ do
    cache <- get
    return $ DataCache.lookup stream cache

cacheInsert :: (Typeable a, Eq (StreamId a), Hashable (StreamId a)) => StreamId a -> Versioned a -> PgStore ()
cacheInsert stream a = PgStore $ modify $ DataCache.insertNotShowable stream a

eventStream :: (Typeable a, Eq (StreamId a), Hashable (StreamId a), Aggregate a, FromJSON (EventT a)) => StreamId a -> PgStore (SVal a)
eventStream stream = do
    maggr <- cacheLookup stream
    case maggr of
        Just _  -> return $ SVal stream
        Nothing -> do
            ver <- getStream stream
            events <- getEvents stream ver
            let aggr = foldE A.empty events
            cacheInsert stream (Versioned ver aggr [])
            return $ SVal stream

rehydrate :: (Typeable a, Aggregate a) => SVal a -> PgStore a
rehydrate (SVal stream) = do
    -- Internal error if this isn't a Just
    Just (Versioned _ a _) <- cacheLookup stream
    return a

applyEvents :: (Typeable a, Eq (StreamId a), Hashable (StreamId a), Aggregate a) => SVal a -> [EventT a] -> PgStore a
applyEvents (SVal stream) es = do
    -- Internal error if this isn't a Just
    Just (Versioned ver a es') <- cacheLookup stream
    let a' = foldE a es
    cacheInsert stream (Versioned ver a' (es' ++ es))
    return a'

----

getStream :: StreamId a -> PgStore Version
getStream stream = do
    conn <- getConn
    tags <- liftIO $ query conn "select tag from event_streams where id = ?" (Only $ streamId stream)
    case tags of
        [Only tag] -> return tag
        _          -> throwStoreE "Event stream not found"

getEvents :: FromJSON (EventT a) => StreamId a -> Version -> PgStore [EventT a]
getEvents stream version = do
    conn <- getConn
    payloads <- liftIO $ query conn "select payload from events where stream_id = ? and version <= ? order by timestamp asc" (streamId stream, version)
    case mapM (fromJSON . fromOnly) payloads of
        Success events -> return events
        Error msg -> throwStoreE msg         -- TODO: Better errors

addEvents :: ToJSON (EventT a) => StreamId a -> [EventT a] -> PgStore ()
addEvents stream events = do
    conn <- getConn
    let payloads = map toJSON events
    _nrows <- liftIO $ executeMany conn "insert into events (stream_id, payload) values (?, ?)" [(streamId stream, p) | p <- payloads]
    return ()
