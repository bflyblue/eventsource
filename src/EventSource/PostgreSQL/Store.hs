{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module EventSource.PostgreSQL.Store where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.Strict
import Data.Aeson
import Data.Either
import Data.Functor.Identity
import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Typeable
import Database.PostgreSQL.Simple
import Haxl.Core.DataCache as DataCache

import EventSource.Aggregate as A

newtype StreamId a = StreamId { streamId :: Int } deriving (Show, Eq, Ord, Hashable)
newtype SVal a = SVal (StreamId a)
type Version = Int

data TState
  = TState
    { tsCache  :: DataCache Identity
    , tsDeltas :: Map.Map Int Delta
    }

data Delta = Delta Version [Value] deriving (Show, Eq)

emptyTState :: TState
emptyTState = TState DataCache.empty Map.empty

newtype PgStore a = PgStore { unPgStore :: RWST Connection () TState (ExceptT String IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

runPgStore :: Connection -> PgStore a -> IO (Either String a)
runPgStore conn a =
    -- uses PostgreSQL's per-connection 'default_transaction_isolation' variable which by
    -- default is ReadCommitted and sufficient for us:
    -- http://www.postgresql.org/docs/9.5/static/transaction-iso.html
    withTransaction conn $ do
        er <- runExceptT (fst <$> evalRWST (unPgStore action) conn emptyTState)
        when (isLeft er) $ rollback conn
        return er
  where
    action = do
        r <- a
        persistChanges
        return r

persistChanges :: PgStore ()
persistChanges = do
    s <- PgStore get
    let changes = filter hasEvents $ Map.toAscList (tsDeltas s)
    forM_ changes $ \(stream, Delta old events) -> do
        updateStream stream old (old + length events)
        addEvents stream old events
    PgStore $ put emptyTState
  where
    hasEvents (_, Delta _ []) = False
    hasEvents _               = True

throwStoreE :: String -> PgStore a
throwStoreE = PgStore . lift . throwE

getConn :: PgStore Connection
getConn = PgStore ask

cacheLookup :: Typeable a => StreamId a -> PgStore (Maybe a)
cacheLookup stream = PgStore $ do
    cache <- gets tsCache
    return $ runIdentity <$> DataCache.lookup stream cache

cacheInsert :: (Typeable a, Eq (StreamId a), Hashable (StreamId a)) => StreamId a -> a -> PgStore ()
cacheInsert stream a = PgStore $ do
    s <- get
    put s { tsCache = DataCache.insertNotShowable stream (Identity a) (tsCache s) }

deltaInit :: StreamId a -> Version -> PgStore ()
deltaInit stream version = PgStore $ do
    s <- get
    put s { tsDeltas = Map.insert (streamId stream) (Delta version []) (tsDeltas s) }

deltaUpdate :: ToJSON (EventT a) => StreamId a -> [EventT a] -> PgStore ()
deltaUpdate stream events = PgStore $ do
    s <- get
    put s { tsDeltas = Map.adjust go (streamId stream) (tsDeltas s) }
  where
    go (Delta v es) = Delta v (es ++ (toJSON <$> events))

eventStream :: (Typeable a, Eq (StreamId a), Hashable (StreamId a), Aggregate a, FromJSON (EventT a)) => StreamId a -> PgStore (SVal a)
eventStream stream = do
    maggr <- cacheLookup stream
    case maggr of
        Just _  -> return $ SVal stream
        Nothing -> do
            ver <- getStream (streamId stream)
            evals <- getEvents (streamId stream) ver
            events <- case mapM fromJSON evals of
                        Success es -> return es
                        Error msg  -> throwStoreE msg
            let aggr = foldE A.empty events
            cacheInsert stream aggr
            deltaInit stream ver
            return $ SVal stream

rehydrate :: (Typeable a, Aggregate a) => SVal a -> PgStore a
rehydrate (SVal stream) = do
    -- Internal error if this isn't a Just
    Just a <- cacheLookup stream
    return a

applyEvents :: (Typeable a, Eq (StreamId a), Hashable (StreamId a), ToJSON (EventT a), Aggregate a) => SVal a -> [EventT a] -> PgStore a
applyEvents (SVal stream) es = do
    -- Internal error if this isn't a Just
    Just a <- cacheLookup stream
    let a' = foldE a es
    cacheInsert stream a
    deltaUpdate stream es
    return a'

----

newStream :: String -> PgStore Int
newStream stype = do
    conn <- getConn
    streams <- liftIO $ query conn "insert into event_streams (type, version) values (?,?) returning id" (stype, 0 :: Int)
    case streams of
        [Only stream] -> return stream
        _             -> throwStoreE "INSERT RETURNING didn't return stream id"

getStream :: Int -> PgStore Version
getStream stream = do
    conn <- getConn
    tags <- liftIO $ query conn "select version from event_streams where id = ?" (Only stream)
    case tags of
        [Only tag] -> return tag
        _          -> throwStoreE "Event stream not found"

updateStream :: Int -> Version -> Version -> PgStore ()
updateStream stream old new = do
    conn <- getConn
    nrows <- liftIO $ execute conn "update event_streams set version = ? where id = ? and version = ?" (new, stream, old)
    when (nrows == 0) $ throwStoreE "Update Conflict"

getEvents :: Int -> Version -> PgStore [Value]
getEvents stream version = do
    conn <- getConn
    vals <- liftIO $ query conn "select payload from events where stream_id = ? and index <= ? order by timestamp asc" (stream, version)
    return $ fromOnly <$> vals

addEvents :: Int -> Version -> [Value] -> PgStore ()
addEvents stream version events = do
    conn <- getConn
    _nrows <- liftIO $ executeMany conn "insert into events (stream_id, index, payload) values (?, ?, ?)" [(stream, version + i, e) | (i, e) <- zip [1..] events]
    return ()
