{-# LANGUAGE OverloadedStrings #-}

module Twilio (sendImage) where

import Control.Lens          ((&), (?~), (.~))
import Data.ByteString.Char8 (pack)
import Data.Monoid           ((<>))
import Network.Wreq
import Data.Text (Text)
import System.Environment (getEnv)

baseUrl :: String
baseUrl = "https://api.twilio.com/2010-04-01"

type ImageUrl = Text
type PhoneNumber = Text
data Credentials = Credentials { keyToken :: String, secretToken :: String }

opts :: Credentials -> PhoneNumber -> Options
opts c phone = defaults & auth ?~ basicAuth (pack $ keyToken c) (pack $ secretToken c)
                  & param "To"   .~ [phone]
                  & param "From" .~ ["+18142834122"]

sendImage :: ImageUrl -> PhoneNumber -> IO ()
sendImage url phone = do
  key <- getEnv "TWILIO_KEY"
  secret <- getEnv "TWILIO_SECRET"
  let creds = Credentials { keyToken = key, secretToken = secret }
      endpoint = baseUrl <> "/Accounts/" <> key <> "/Messages.json"
  response     <- postWith (opts creds phone) endpoint ["From"     := ("+18142834122" :: String),
                                                        "To"       := ("+18143860516" :: String),
                                                        "MediaUrl" := url]
  jsonResponse <- asValue response
  print jsonResponse
