module Main where

import Network.HTTP.Req (runReq, defaultHttpConfig)
import Control.Monad.IO.Class (liftIO)

import Helpers


main = runReq defaultHttpConfig $ do
    channels <- fetchFeed
    liftIO $ print (head channels)