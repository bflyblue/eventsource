module Eventstore.PostgreSQL.Internal.Store where

import           Control.Monad
import           Control.Monad.Trans.RWS.Strict             (evalRWST, get, put)
import qualified Data.Map.Strict                            as Map
import           Database.PostgreSQL.Simple                 (Connection, withTransaction)

import           Eventstore.PostgreSQL.Internal.EventStream
import           Eventstore.PostgreSQL.Internal.Types

-- uses PostgreSQL's per-connection 'default_transaction_isolation' variable which by
-- default is ReadCommitted and sufficient for us:
-- http://www.postgresql.org/docs/9.5/static/transaction-iso.html
runPgStore :: Connection -> PgStore a -> IO a
runPgStore conn a =
    withTransaction conn $ fst <$> evalRWST (unPgStore action) conn emptyPgState
  where
    action = do
        r <- a
        persistChanges
        return r

runPgStore' :: Connection -> PgStore a -> IO a
runPgStore' conn a = fst <$> evalRWST (unPgStore a) conn emptyPgState

persistChanges :: PgStore ()
persistChanges = do
    s <- PgStore get
    let changes = filter hasEvents $ Map.toAscList (sDeltas s)
    d <- forM changes $ \(stream, Delta old snap events) -> do
        let new = old + length events
        updateStream stream old new
        addEvents stream old events
        return (stream, Delta new snap [])
    PgStore $ put s { sDeltas = Map.fromList d }
  where
    hasEvents (_, Delta _ _ []) = False
    hasEvents _                 = True
