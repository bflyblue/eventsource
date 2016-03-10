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

rehydrate :: (Typeable a, Aggregate a, FromJSON (EventT a)) => StreamId a -> PgStore a
rehydrate stream = do
    maggr <- cacheLookup stream
    case maggr of
        Just a  -> return a
        Nothing -> do
            ver <- getStream (streamId stream)
            jsonEvents <- getEvents (streamId stream) ver
            events <- case mapM fromJSON jsonEvents of
                        Success es -> return es
                        Error msg  -> throwError msg
            let a = foldE A.empty events
            cacheInsert stream a
            deltaInit stream ver
            return a

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
    tags <- liftIO $ query conn "select version from event_streams where id = ?" (Only stream)
    case tags of
        [Only tag] -> return tag
        _          -> throwError "Event stream not found"

updateStream :: Int -> Version -> Version -> PgStore ()
updateStream stream old new = do
    conn <- getConn
    nrows <- liftIO $ execute conn "update event_streams set version = ? where id = ? and version = ?" (new, stream, old)
    when (nrows == 0) $ throwError "Update Conflict"

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
