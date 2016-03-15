{-# LANGUAGE OverloadedStrings #-}
module ImageWriter (writeImage) where

import Data.ByteString.Lazy (writeFile, ByteString)
import Prelude hiding       (writeFile) 
import System.Directory     (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath      ((</>))
import Data.Monoid          ((<>))

type ImageId = String
type Image   = ByteString

-- Write image to himages directory given image id
writeImage :: ImageId -> Image -> IO ()
writeImage imgId img = do
  dir <- getCurrentDirectory
  let hImageDir = dir </> "frontend" </> "dist" </> "himages"
      imagePath = hImageDir </> imgId <> ".png"
  createDirectoryIfMissing False hImageDir
  writeFile imagePath img
