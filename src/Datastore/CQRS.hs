module Datastore.CQRS
( Command
, Query
, runCommand
, runQuery
)
where

import Datastore.CQRS.Command (Command, runCommand)
import Datastore.CQRS.Query   (Query,   runQuery)
