{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson
import Data.Aeson.TH
import Network.Handwriting
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

type API = "handwritings" :> Get '[JSON] [Handwriting] :<|> Raw

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = allHandwritings :<|> serveDirectory "frontend/dist/"

allHandwritings :: EitherT ServantErr IO [Handwriting]
allHandwritings = do
  key <- liftIO $ getEnv "KEY"
  secret <- liftIO $ getEnv "SECRET"
  let creds = Credentials key secret
  liftIO $ getHandwritings creds
