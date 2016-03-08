{-# LANGUAGE TypeFamilies          #-}

module EventSource.Store where

import Control.Monad.IO.Class

class Store s where
    transaction :: MonadIO m => s a -> m (Either String a)
    rollback :: String -> s a
