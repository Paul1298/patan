{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Codec.Xlsx
import           Control.Lens
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  bs <- L.readFile "Летальность.xlsx"
  let value = toXlsx bs ^? ixSheet "Лист1" .
              ixCell (1, 1) . cellValue . _Just
  putStrLn $ show value
