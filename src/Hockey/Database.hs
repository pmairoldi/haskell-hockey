{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hockey.Database (

)

where

-- import Control.Monad.IO.Class  (liftIO)
-- import Database.Persist
-- import Database.Persist.Postgresql
-- import Database.Persist.TH
--
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
--     Person
--         name String
--         age Int Maybe
--         deriving Show
--     BlogPost
--         title String
--         authorId PersonId
--         deriving Show
--     |]
--
-- connStr = "host=localhost dbname=pierremarcairoldi user=pierremarcairoldi port=5432"
