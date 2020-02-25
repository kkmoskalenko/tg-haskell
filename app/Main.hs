module Main where

import Network.HTTP.Req (runReq, defaultHttpConfig)
import Control.Monad (forever)

import Helpers


startPolling sendFunc = forever $ do
    news <- relevantNews
    mapM sendFunc news
    delayPoll


main = runReq defaultHttpConfig $ do
    bot <- initBot
    startPolling (sendItem bot)