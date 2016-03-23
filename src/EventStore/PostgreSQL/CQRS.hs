module Eventstore.PostgreSQL.CQRS
( Command
, Query
, runCommand
, runQuery
)
where

import Eventstore.PostgreSQL.Internal.Command (Command, runCommand)
import Eventstore.PostgreSQL.Internal.Query   (Query,   runQuery)
