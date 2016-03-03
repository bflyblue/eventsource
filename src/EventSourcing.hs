{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module EventSourcing where

import           Control.Arrow              (returnA)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.List                  (foldl')
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics
import           Opaleye
import           Version

data StoreEnv = StoreEnv { pgconn :: Connection }
data StoreErr = StoreErr String

type Store = ReaderT StoreEnv (EitherT StoreErr IO)

runStore :: StoreEnv -> Store a -> IO (Either StoreErr a)
runStore env = runEitherT . flip runReaderT env

storeErr :: StoreErr -> Store a
storeErr = lift . left

withPgConn :: (Connection -> IO a) -> Store a
withPgConn a = do
    conn <- asks pgconn
    liftIO $ a conn

type Ver = Int

class Aggregate a where
    data EventT a
    version :: a -> Ver
    empty   :: a
    apply   :: EventT a -> a -> a
    foldE   :: (Aggregate a, Traversable t) => t (EventT a) -> a

    foldE = foldl' (flip apply) empty

class Aggregate (AggregateT a) => EventStream a where
    type AggregateT a
    appendEvents     :: a -> [EventT (AggregateT a)] -> Store ()
    fetchEvents      :: a -> Store [EventT (AggregateT a)]
    fetchEventsRange :: Ver -> Ver -> a -> Store [EventT (AggregateT a)]

    fetchEvents = fetchEventsRange 0 maxBound

    fetchEventsRange first last_ a | last_ >= first = take (last_ - first) . drop first <$> fetchEvents a
                                   | otherwise      = storeErr (StoreErr "Invalid range")

rehydrate :: EventStream a => a -> Store (AggregateT a)
rehydrate stream = do
    events <- fetchEvents stream
    return $! foldE events

data PersonEvent' a b c = PersonEvent
  { personEventStreamId  :: a
  , personEventTimestamp :: b
  , personEventPayload   :: c
  } deriving (Show, Eq, Generic)

type PersonEvent       = PersonEvent' Int UTCTime Value
type PersonEventColumn = PersonEvent' (Column PGInt4) (Column PGTimestamptz) (Column PGJsonb)

instance FromJSON PersonEvent
instance ToJSON PersonEvent

$(makeAdaptorAndInstance "pPersonEvent" ''PersonEvent')

personEventTable :: Table (PersonEvent' (Column PGInt4) (Maybe (Column PGTimestamptz)) (Column PGJsonb)) PersonEventColumn
personEventTable =
    Table "person_events"
        (pPersonEvent PersonEvent { personEventStreamId  = required "stream_id"
                                  , personEventTimestamp = optional "timestamp"
                                  , personEventPayload   = required "payload"
                                  })

personEventQuery :: Query PersonEventColumn
personEventQuery = queryTable personEventTable


data Person = Person String

instance Aggregate (Versioned Person) where
    data EventT (Versioned Person) = UpdatePerson String deriving (Show, Eq, Generic)
    version = Version.version
    empty = Initial
    apply (UpdatePerson name) = vadjust (const $ Person name)

instance FromJSON (EventT (Versioned Person))
instance ToJSON (EventT (Versioned Person))

newtype PersonEventStream = PersonEventStream Int

instance EventStream PersonEventStream where
    type AggregateT PersonEventStream = Versioned Person

    appendEvents (PersonEventStream stream) events =
        withPgConn $ \conn -> do
            _ <- runInsertMany conn personEventTable (jsonEvent <$> events)
            return ()
        where
            jsonEvent e = PersonEvent (pgInt4 stream) Nothing (pgValueJSONB e)

    fetchEvents (PersonEventStream stream) = do
        events <- withPgConn $ \conn ->
            runQuery conn $ proc () -> do
                e <- personEventQuery -< ()
                restrict -< personEventStreamId e .== pgInt4 stream
                returnA -< personEventPayload e
        mapM parseEvent events
        where
            parseEvent v = case fromJSON v of
                               Success e -> return e
                               Error msg -> storeErr (StoreErr $ "JSON parse failure: " ++ msg)
