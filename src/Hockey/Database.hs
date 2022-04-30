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

upsertMany :: (MonadIO m, PersistUniqueWrite backend, OnlyOneUniqueKey record, PersistEntityBackend record ~ BaseBackend backend) => [record] -> ReaderT backend m ()
upsertMany [] = return ()
upsertMany (x:xs) = do
    upsert x []
    upsertMany xs

insertManyUnique :: (MonadIO m, PersistUniqueWrite backend, PersistEntity record, PersistEntityBackend record ~ BaseBackend backend) => [record] -> ReaderT backend m ()
insertManyUnique [] = return ()
insertManyUnique (x:xs) = do
    insertUnique x
    insertManyUnique xs
