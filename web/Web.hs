{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Yesod
import Handlers
import Hockey.Environment

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/Hockey/Playoffs LegacyPlayoffsR GET
/playoffs PlayoffsR GET
/playoffs/#Integer PlayoffsWithYearR GET
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
