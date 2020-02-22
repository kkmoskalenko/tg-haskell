{-# LANGUAGE OverloadedStrings #-}

module Helpers
( fetchFeed
, getLastPollingTime
, updateLastPollingTime
) where

import Network.HTTP.Req
import Text.XML.Light (parseXML)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing, fromJust)
import System.Exit (exitFailure)

import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)

import Data.Time
import Data.Time.Clock.System
import Data.Int (Int64)
import Control.Exception (catch, SomeException)

import RSS


-- http://fit.nsu.ru/component/ninjarsssyndicator?feed_id=1&format=raw
feedUrl = http "fit.nsu.ru" /: "component" /: "ninjarsssyndicator"
feedQuery = "feed_id" =: (1 :: Int) <> "format" =: ("raw" :: String)


fetchFeed :: Req [Channel]
fetchFeed = do
    -- Получаем данные RSS-ленты
    res <- req GET feedUrl NoReqBody bsResponse feedQuery
    let feed = toLazyText $ htmlEncodedText $ decodeUtf8 (responseBody res)

    -- Находим корневой тег "rss", в противном случае завершаем программу
    let root = findRoot $ parseXML feed
    when (isNothing root) $ liftIO $ do putStrLn "Root RSS node not found!" ; exitFailure

    return $ parseChannels (fromJust root)


lastPollingTimeFilePath = "lastPollingTime.txt"

getLastPollingTime :: IO UTCTime
getLastPollingTime = catch funcBody handler where
    funcBody = do
        contents <- readFile lastPollingTimeFilePath
        let seconds = read contents :: Int64
        let systemTime = MkSystemTime
              { systemSeconds = seconds
              , systemNanoseconds = 0 }
        return $ systemToUTCTime systemTime
    handler :: SomeException -> IO UTCTime
    handler _ = do
        systemTime <- getSystemTime
        return $ systemToUTCTime systemTime

updateLastPollingTime :: IO ()
updateLastPollingTime = do
    systemTime <- getSystemTime
    writeFile lastPollingTimeFilePath (show $ systemSeconds systemTime)