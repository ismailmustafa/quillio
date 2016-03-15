{-# LANGUAGE OverloadedStrings #-}

module Twilio (sendImage) where

import Control.Lens          ((&), (?~), (.~))
import Data.ByteString.Char8 (pack)
import Data.Monoid           ((<>))
import Network.Wreq
import System.Environment (getEnv)

baseUrl :: String
baseUrl = "https://api.twilio.com/2010-04-01"

type ImageUrl = String
type PhoneNumber = String
data Credentials = Credentials { keyToken :: String, secretToken :: String }

opts :: Credentials -> Options
opts c = defaults & auth ?~ basicAuth (pack $ keyToken c) (pack $ secretToken c)

-- Send image through twilio given image url and phone number
sendImage :: ImageUrl -> PhoneNumber -> IO ()
sendImage url phone = do
  key <- getEnv "TWILIO_KEY"
  secret <- getEnv "TWILIO_SECRET"
  let creds = Credentials { keyToken = key, secretToken = secret }
      endpoint = baseUrl <> "/Accounts/" <> key <> "/Messages.json"
  postWith (opts creds) endpoint ["From"     := ("+18142834122" :: String),
                                  "To"       := phone,
                                  "MediaUrl" := url]
  return ()
