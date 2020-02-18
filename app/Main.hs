module Main where

import Network.HTTP
import Text.XML.Light (parseXML)

import Control.Monad
import Data.Maybe
import System.Exit
import System.IO

import Data.Text (pack)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)

import RSS


feedUrl = "feed://fit.nsu.ru/component/ninjarsssyndicator/?feed_id=1&format=raw"

main = do
    -- Получаем данные RSS-ленты
    feed' <- simpleHTTP (getRequest feedUrl) >>= getResponseBody
    let feed = toLazyText $ htmlEncodedText $ pack feed'

    -- Находим корневой тег "rss", в противном случае завершаем программу
    let root = findRoot $ parseXML feed
    when (isNothing root) $ do putStrLn "Root RSS node not found!" ; exitFailure

    let channels = parseChannels (fromJust root)
    
    hSetEncoding stdout latin1
    putStrLn $ show (head channels)