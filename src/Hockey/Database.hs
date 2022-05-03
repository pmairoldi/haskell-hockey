{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Hockey.Database (
    module Hockey.Database.Types,
    module Hockey.Database.Internal,
    module Database.Persist,
    upsertMany,
    insertManyUnique
)

where

import Hockey.Database.Internal
import Hockey.Database.Types
import Database.Persist
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad (liftM)

insertOrReplace :: (MonadIO m, PersistUniqueWrite backend, OnlyOneUniqueKey record, PersistEntityBackend record ~ BaseBackend backend) => record -> ReaderT backend m ()
insertOrReplace record = do
    uniqueKey <- onlyUnique record
    mExists <- getBy uniqueKey
    case mExists of
        Just (Entity k _) -> replace k record
        Nothing           -> insert record >> return ()

upsertMany :: (MonadIO m, PersistUniqueWrite backend, OnlyOneUniqueKey record, PersistEntityBackend record ~ BaseBackend backend) => [record] -> ReaderT backend m ()
upsertMany [] = return ()
upsertMany (x:xs) = do
    insertOrReplace x
    upsertMany xs

insertManyUnique :: (MonadIO m, PersistUniqueWrite backend, PersistEntity record, PersistEntityBackend record ~ BaseBackend backend) => [record] -> ReaderT backend m ()
insertManyUnique [] = return ()
insertManyUnique (x:xs) = do
    insertUnique x
    insertManyUnique xs
