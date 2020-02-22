{-# LANGUAGE OverloadedStrings #-}

module Helpers (fetchFeed) where

import Network.HTTP.Req
import Text.XML.Light (parseXML)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing, fromJust)
import System.Exit (exitFailure)

import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)

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