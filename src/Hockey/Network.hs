module Hockey.Network (
    ReturnType(..),
    get
) where

import Network.HTTP
import Data.List as List

data ReturnType = JSON | JSONP | HTML deriving (Enum, Show, Eq)

get :: String -> ReturnType -> IO String
get url responseType
    | responseType == JSONP = jsonpRequestParse response
    | otherwise = requestParse response
    where response = openUrl url

openUrl :: String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody

jsonpRequestParse :: IO String -> IO String
jsonpRequestParse response = do
    body <- response
    return $ jsonpToJson body

requestParse :: IO String -> IO String
requestParse response = do
    body <- response
    return $ body

dropAndReverse :: Eq a => a -> [a] -> [a]
dropAndReverse delimiter content = List.reverse (List.dropWhile (/= delimiter) content)

jsonpToJson :: String -> String
jsonpToJson jsonp
        | and $ [("{" `List.isPrefixOf` jsonp), ("}" `List.isSuffixOf` jsonp)] = jsonp
        | otherwise = dropAndReverse '}' (dropAndReverse '{' jsonp)
