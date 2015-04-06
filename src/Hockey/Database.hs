{-# LANGUAGE FlexibleContexts #-}

module Hockey.Database (
    module Hockey.Database.Types,
    module Hockey.Database.Formatting,
    insertGames,
    postgres,
    sqlite,
    run
)

where

import Hockey.Database.Internal
import Hockey.Database.Formatting
import Hockey.Database.Types
-- db actions

insertGames [] = return ()
insertGames (x:xs) = do
    upsert x []
    insertGames xs
