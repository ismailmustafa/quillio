{-# LANGUAGE OverloadedStrings #-}

module Twilio (sendImage) where

import Control.Lens          ((&), (?~), (.~))
import Data.ByteString.Char8 (pack)
import Data.Monoid           ((<>))
import Network.Wreq
import System.Environment (getEnv)

baseUrl :: String
baseUrl = "https://api.twilio.com/2010-04-01"

data Credentials = Credentials { keyToken :: String, secretToken :: String }

opts :: Credentials -> Options
opts c = defaults & auth ?~ basicAuth (pack $ keyToken c) (pack $ secretToken c)
                  & param "To" .~ ["+18143860516"]
                  & param "From" .~ ["+18142834122"]

type ImageUrl = String

sendImage :: ImageUrl -> IO ()
sendImage url = do
  key <- getEnv "TWILIO_KEY"
  secret <- getEnv "TWILIO_SECRET"
  let creds = Credentials { keyToken = key, secretToken = secret }
      endpoint = baseUrl <> "/Accounts/" <> key <> "/Messages.json"
  response     <- postWith (opts creds) endpoint ["From"     := ("+18142834122" :: String),
                                                  "To"       := ("+18143860516" :: String),
                                                  "MediaUrl" := url]
  jsonResponse <- asValue response
  print jsonResponse
