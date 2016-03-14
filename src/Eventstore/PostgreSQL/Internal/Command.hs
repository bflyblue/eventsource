{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eventstore.PostgreSQL.Internal.Command
( Command(..)
, command
, runCommand
)
where

import Database.PostgreSQL.Simple (Connection)
import Eventstore.PostgreSQL.Internal.Store
import Eventstore.PostgreSQL.Internal.Types

newtype Command a = Command (PgStore a) deriving (Functor, Applicative, Monad)

command :: PgStore a -> Command a
command = Command

runCommand :: Connection -> Command a -> IO a
runCommand conn (Command action) = runPgStore conn action
