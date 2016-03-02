module Datastore.Command where

import           Data.Text                  (Text)

type Command = IO

createPerson :: Text -> Command Int
createPerson = undefined
