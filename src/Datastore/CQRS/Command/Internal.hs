{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Datastore.CQRS.Command.Internal
( Command(..)
, command
, runCommand
)
where

import Database.PostgreSQL.Simple (Connection)
import EventStore.PostgreSQL

newtype Command a = Command (PgStore a) deriving (Functor, Applicative, Monad)

command :: PgStore a -> Command a
command = Command

runCommand :: Connection -> Command a -> IO a
runCommand conn (Command action) = runPgStore conn action
