{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Handlers.Playoffs
import Hockey.Environment
import Yesod

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/Hockey/Playoffs PlayoffsR GET
|]

cors
  :: Yesod site
  => HandlerT site IO res -> HandlerT site IO res
cors handler = do
  addHeader "Access-Control-Allow-Origin" "*"
  handler

instance Yesod App where
  yesodMiddleware = cors . defaultYesodMiddleware

main :: IO ()
main = do
  e <- env
  warp (port e) App
