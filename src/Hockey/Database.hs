{-# LANGUAGE FlexibleContexts #-}

module Hockey.Database (
    module Hockey.Database.Types,
    upsertMany,
    connect,
    Database(..),
    DatabaseType(..)
)

where

import Hockey.Database.Internal
import Hockey.Database.Types

upsertMany [] = return ()
upsertMany (x:xs) = do
    upsert x []
    upsertMany xs
