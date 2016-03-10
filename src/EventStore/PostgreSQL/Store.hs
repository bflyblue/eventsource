{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module EventStore.PostgreSQL.Store where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.RWS.Strict
import           Data.Aeson
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.Map.Strict                as Map
import           Data.Typeable
import           Database.PostgreSQL.Simple
import           Haxl.Core.DataCache            as DataCache

import           EventStore.Aggregate           as A

newtype StreamId a = StreamId { streamId :: Int } deriving (Show, Eq, Ord, Hashable)
type Version = Int

data PgState = PgState
    { sCache  :: DataCache Identity
    , sDeltas :: Map.Map Int Delta
    }

data Delta = Delta Version [Value] deriving (Show, Eq)

data PgStoreError = InternalError String deriving (Show, Eq, Ord)
instance Exception PgStoreError

emptyPgState :: PgState
emptyPgState = PgState DataCache.empty Map.empty

newtype PgStore a = PgStore
    { unPgStore :: RWST Connection () PgState IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

runPgStore :: Connection -> PgStore a -> IO a
runPgStore conn a =
    -- uses PostgreSQL's per-connection 'default_transaction_isolation' variable which by
    -- default is ReadCommitted and sufficient for us:
    -- http://www.postgresql.org/docs/9.5/static/transaction-iso.html
    withTransaction conn $ fst <$> evalRWST (unPgStore action) conn emptyPgState
  where
    action = do
        r <- a
        persistChanges
        return r

persistChanges :: PgStore ()
persistChanges = do
    s <- PgStore get
    let changes = filter hasEvents $ Map.toAscList (sDeltas s)
    d <- forM changes $ \(stream, Delta old events) -> do
        let new = old + length events
        updateStream stream old new
        addEvents stream old events
        return (stream, Delta new [])
    PgStore $ put s { sDeltas = Map.fromList d }
  where
    hasEvents (_, Delta _ []) = False
    hasEvents _               = True

throwError :: String -> PgStore a
throwError = PgStore . liftIO . throw . InternalError

getConn :: PgStore Connection
getConn = PgStore ask

cacheLookup :: Typeable a => StreamId a -> PgStore (Maybe a)
cacheLookup stream = PgStore $ do
    cache <- gets sCache
    return $ runIdentity <$> DataCache.lookup stream cache

cacheInsert :: (Typeable a, Eq (StreamId a), Hashable (StreamId a)) => StreamId a -> a -> PgStore ()
cacheInsert stream a = PgStore $ do
    s <- get
    put s { sCache = DataCache.insertNotShowable stream (Identity a) (sCache s) }

deltaInit :: StreamId a -> Version -> PgStore ()
deltaInit stream version = PgStore $ do
    s <- get
    put s { sDeltas = Map.insert (streamId stream) (Delta version []) (sDeltas s) }

deltaLookup :: StreamId a -> PgStore (Maybe Delta)
deltaLookup stream = PgStore $ do
    s <- get
    return $ Map.lookup (streamId stream) (sDeltas s)

deltaInsert :: StreamId a -> Delta -> PgStore ()
deltaInsert stream delta = PgStore $ do
    s <- get
    put s { sDeltas = Map.insert (streamId stream) delta (sDeltas s) }

rehydrate :: (Typeable a, Aggregate a, FromJSON a, FromJSON (EventT a)) => StreamId a -> PgStore a
rehydrate stream = do
    maggr <- cacheLookup stream
    case maggr of
        Just a  -> return a
        Nothing -> do
            (ver, snap) <- getStreamSnap (streamId stream)
            (a, es) <- case snap of
                Just snapver -> do
                    s <- getSnapshot (streamId stream) snapver
                    jsonEvents <- getEventsRange (streamId stream) snapver ver
                    s' <- fromResult (fromJSON s)
                    return (s', jsonEvents)
                Nothing -> do
                    jsonEvents <- getEvents (streamId stream) ver
                    return (A.empty, jsonEvents)
            events <- fromResult (mapM fromJSON es)
            let a' = foldE a events
            cacheInsert stream a'
            deltaInit stream ver
            return a'
  where
    fromResult (Success r) = return r
    fromResult (Error msg) = throwError msg

snapshot :: (Typeable a, Aggregate a, FromJSON (EventT a), FromJSON a, ToJSON a) => StreamId a -> PgStore ()
snapshot stream = do
    a <- rehydrate stream
    Just (Delta ver _) <- deltaLookup stream
    snapshotStream (streamId stream) ver (toJSON a)

applyEvents :: (Typeable a, Eq (StreamId a), Hashable (StreamId a), ToJSON (EventT a), Aggregate a) => StreamId a -> [EventT a] -> PgStore ()
applyEvents stream events = do
    let jsonEvents = toJSON <$> events
    mdelta <- deltaLookup stream
    delta  <- case mdelta of
                Just (Delta ver es) -> return $ Delta ver (es ++ jsonEvents)
                Nothing             -> Delta <$> getStream (streamId stream) <*> pure jsonEvents
    deltaInsert stream delta

    maggr <- cacheLookup stream
    case maggr of
        Just a  -> cacheInsert stream (foldE a events)
        Nothing -> return ()

----

newStream :: String -> PgStore Int
newStream stype = do
    conn <- getConn
    streams <- liftIO $ query conn "insert into event_streams (type, version) values (?,?) returning id" (stype, 0 :: Int)
    case streams of
        [Only stream] -> return stream
        _             -> throwError "INSERT RETURNING didn't return stream id"

getStream :: Int -> PgStore Version
getStream stream = do
    conn <- getConn
    vers <- liftIO $ query conn "select version from event_streams where id = ?" (Only stream)
    case vers of
        [Only ver] -> return ver
        _          -> throwError "Event stream not found"

getStreamSnap :: Int -> PgStore (Version, Maybe Version)
getStreamSnap stream = do
    conn <- getConn
    vers <- liftIO $ query conn "select version, snapshot from event_streams where id = ?" (Only stream)
    case vers of
        [verpair] -> return verpair
        _         -> throwError "Event stream not found"

updateStream :: Int -> Version -> Version -> PgStore ()
updateStream stream old new = do
    conn <- getConn
    nrows <- liftIO $ execute conn "update event_streams set version = ? where id = ? and version = ?" (new, stream, old)
    when (nrows == 0) $ throwError "Update Conflict"

snapshotStream :: Int -> Version -> Value -> PgStore ()
snapshotStream stream version value = do
    conn <- getConn
    _ <- liftIO $ execute conn "insert into snapshots (stream_id, version, payload) values (?,?,?) on conflict (stream_id, version) do update set payload = ?"
                               (stream, version, value, value)
    _ <- liftIO $ execute conn "update event_streams set snapshot = ? where id = ? and coalesce(snapshot, 0) < ?" (version, stream, version)
    return ()

getSnapshot :: Int -> Version -> PgStore Value
getSnapshot stream version = do
    conn <- getConn
    vals <- liftIO $ query conn "select payload from snapshots where stream_id = ? and version = ?" (stream, version)
    case vals of
        [Only val] -> return val
        _          -> throwError "Snapshot not found"

getEvents :: Int -> Version -> PgStore [Value]
getEvents stream version = do
    conn <- getConn
    vals <- liftIO $ query conn "select payload from events where stream_id = ? and index <= ? order by index asc" (stream, version)
    return $ fromOnly <$> vals

getEventsRange :: Int -> Version -> Version -> PgStore [Value]
getEventsRange stream after till = do
    conn <- getConn
    vals <- liftIO $ query conn "select payload from events where stream_id = ? and index > ? and index <= ? order by index asc" (stream, after, till)
    return $ fromOnly <$> vals

addEvents :: Int -> Version -> [Value] -> PgStore ()
addEvents stream version events = do
    conn <- getConn
    _nrows <- liftIO $ executeMany conn "insert into events (stream_id, index, payload) values (?, ?, ?)" [(stream, version + i, e) | (i, e) <- zip [1..] events]
    return ()
