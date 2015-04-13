{-# LANGUAGE FlexibleContexts #-}

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

upsertMany :: (MonadIO m, PersistEntity val, PersistUnique (PersistEntityBackend val)) => [val] -> ReaderT (PersistEntityBackend val) m ()
upsertMany [] = return ()
upsertMany (x:xs) = do
    upsert x []
    upsertMany xs

insertManyUnique :: (MonadIO m, PersistEntity val, PersistUnique (PersistEntityBackend val)) => [val] -> ReaderT (PersistEntityBackend val) m ()
insertManyUnique [] = return ()
insertManyUnique (x:xs) = do
    insertUnique x
    insertManyUnique xs
