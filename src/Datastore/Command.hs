module Datastore.Command where

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

data CmdErr = CmdErr

data CmdEnv = CmdEnv { connection :: Connection }

type Command = ReaderT CmdEnv (EitherT CmdErr IO)

runCommand :: CmdEnv -> Command a -> IO (Either CmdErr a)
runCommand env cmd = runEitherT $ runReaderT cmd env

getConnection :: Command Connection
getConnection = asks connection

createPerson :: Text -> Command Int
createPerson _name = do
    _conn <- getConnection
    -- liftIO $ runInsert conn
    return 1
