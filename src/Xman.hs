{-# LANGUAGE OverloadedStrings #-}
module Xman where
import           Codec.Xlsx
import           Codec.Xlsx.Writer.Internal (txtd)
import           Control.Lens
import qualified Data.ByteString.Lazy       as L
import           Data.List                  (sort)
import           Data.Text                  (unpack)

getColumn :: Int -> IO [String]
getColumn col = do
  bs <- L.readFile "Летальность.xlsx"
  let value r c = case help of
                  Just (CellText t)   -> t
                  Just (CellDouble d) -> txtd d
                  _                   -> ""
                  where
                    help = toXlsx bs ^? ixSheet "Лист1"
                           . ixCell (r, c) . cellValue . _Just
  return $ [unpack $ value r col | r <- [2..4]] -- TODO ? sort

fio :: IO [String]
fio = getColumn 2

number :: IO [String]
number = getColumn 8

department :: IO [String]
department = getColumn 7

getFIO :: Int -> IO String
getFIO row = do
  bs <- L.readFile "Летальность.xlsx"
  let value = let Just (CellText t) = help
              in unpack t
              where
                help = toXlsx bs ^? ixSheet "Лист1"
                       . ixCell (row, 2) . cellValue . _Just
  return value
