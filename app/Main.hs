-- {-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.UTF8(toString)
import Data.Either as DE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLU
import qualified Data.ByteString.Char8 as BSU
import System.Environment (getArgs)

import Template

path = "example.docx"

main :: IO ()
main = do
  n <- getArgs
  let name = doc $ plain $ str tes
  -- docxfile <- runIO (writeDocx def name) >>= handleError
  docxfile <- runIO (writeDocx def{writerReferenceDoc = Just "in.docx"} name) >>= handleError
  BL.writeFile path docxfile
  putStrLn "Created example.docx"
