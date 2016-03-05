module EventSource.Store where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader

type StoreT env err m = ReaderT env (EitherT err m)

runStoreT :: env -> StoreT env err m a -> m (Either err a)
runStoreT env = runEitherT . flip runReaderT env

storeErr :: Monad m => err -> StoreT env err m a
storeErr = lift . left
