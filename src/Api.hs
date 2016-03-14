{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Api
    ( startApp
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (pack)
import Data.Monoid ((<>))
import Network.Handwriting
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Base64 as B
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Twilio
import ImageWriter

baseUrl :: String
baseUrl = "http://ec2-52-32-2-100.us-west-2.compute.amazonaws.com"

type API = "api" :> "image" :> QueryParam "red" Word8
                   :> QueryParam "green" Word8
                   :> QueryParam "blue" Word8
                   :> QueryParam "imageId" String
                   :> QueryParam "handwritingId" String
                   :> QueryParam "text" String
                   :> Get '[OctetStream] BS.ByteString
         :<|> "api" :> "handwritings" :> Get '[JSON] [Handwriting]
         :<|> "api" :> "sendImage" :> QueryParam "imageId" String
                                   :> QueryParam "phoneNumber" String
                                   :> Post '[JSON] ()
         :<|> Raw

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getImage :<|> allHandwritings :<|> sendMMS :<|> serveDirectory "frontend/dist/"

allHandwritings :: EitherT ServantErr IO [Handwriting]
allHandwritings = do
  key <- liftIO $ getEnv "HANDWRITING_KEY"
  secret <- liftIO $ getEnv "HANDWRITING_SECRET"
  let creds = Credentials key secret
  liftIO $ getHandwritings creds

getImage :: Maybe Word8 -> Maybe Word8 -> Maybe Word8 -> Maybe String -> Maybe String -> Maybe String -> EitherT ServantErr IO BS.ByteString
getImage r g b imgS s t = do
  key <- liftIO $ getEnv "HANDWRITING_KEY"
  secret <- liftIO $ getEnv "HANDWRITING_SECRET"
  let creds = Credentials key secret
      rgb = (fromMaybe 0 r,fromMaybe 0 g,fromMaybe 0 b) :: Color
      handId = fromMaybe "31SB2CWG00DZ" s
      imageId = fromMaybe "" imgS
      text = fromMaybe "" t
      params = defaultImageParams { color = Just rgb
                                  , hId = Just handId
                                  , width = Just 800
                                  , height = Just 486
                                  , size = Just 60 }
  image <- liftIO $ renderImage creds params text
  liftIO $ writeImage imageId image
  let base64Image = B.encode $ BSL.toStrict image
  return base64Image

sendMMS :: Maybe String -> Maybe String -> EitherT ServantErr IO ()
sendMMS imageId phoneNumber = do
  let imgId = fromMaybe "" imageId
      phone = pack $ fromMaybe "" phoneNumber
      imageUrl =  pack $ baseUrl <> "/himages/" <> imgId <> ".png"
  liftIO $ sendImage imageUrl phone
