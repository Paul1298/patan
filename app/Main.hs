-- {-# LANGUAGE OverloadedStrings #-}
module Main where
import Template
import Writer

import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E

main :: IO ()
main = do
  writeRTF
