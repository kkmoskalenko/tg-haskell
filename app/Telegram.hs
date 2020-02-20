{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram
( Bot (..)
, testToken
, sendMessage
) where

import Network.HTTP.Req
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)


data Bot = Bot { token :: String }

data Status = Status { ok :: Bool }
    deriving (Generic)

instance FromJSON Status


baseUrl :: Bot -> Url 'Https
baseUrl (Bot token) = https "api.telegram.org" /~ ("bot" ++ token)


testToken :: Bot -> Req Bool
testToken bot = do
    res <- req GET (baseUrl bot /: "getMe") NoReqBody jsonResponse mempty
    return $ ok (responseBody res :: Status)


sendMessage :: Bot -> String -> String -> Req Bool
sendMessage bot chatId text = do
    let options =
          "chat_id" =: chatId <> "text" =: text <>
          "parse_mode" =: ("Markdown" :: String)
    res <- req GET (baseUrl bot /: "sendMessage") NoReqBody jsonResponse options
    return $ ok (responseBody res :: Status)