{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Pool
import Datastore.Aggregates.Person
import Database.PostgreSQL.Simple
import EventStore.PostgreSQL.Store


main :: IO ()
main = do
    let conninfo = defaultConnectInfo { connectDatabase = "store"
                                      , connectHost     = "neptune"
                                      , connectUser     = "shaun"
                                      , connectPassword = "icecream" }

    pool <- createPool (connect conninfo) close 1 5 20

    shaun <- withResource pool $ flip runPgStore (initPerson (Person "Shaun" 39))

    asyncs <- replicateM 20 $ async (updateThread pool shaun)
    mapM_ wait asyncs

updateThread :: Pool Connection -> PersonId -> IO ()
updateThread pool person = forM_ [1..1000] $ \i -> do
    go (i :: Int)
    threadDelay 1
  where
    go i =
        withResource pool $ \conn -> do
            p <- runPgStore conn $ do
                when (i `mod` 10 == 0) (snapshot person)
                rehydrate person
            print p

            u <- try $ runPgStore conn $ do
                applyEvents person [ChangedName "Shaun Sharples", ChangedAge 21]
                applyEvents person [ChangedAge 39]
            case u of
                Left (InternalError msg) -> do
                    putStrLn $ "Update failed: " ++ msg ++ ", retrying..."
                    go (succ i)
                Right () -> putStrLn "Update successful"
