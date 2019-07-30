{-# LANGUAGE OverloadedStrings #-}
module Xman where
import           Codec.Xlsx
import           Codec.Xlsx.Writer.Internal (txtd)
import           Control.Lens
import qualified Data.ByteString.Lazy       as L
import           Data.List                  (sort)
import           Data.Text                  (Text, pack)

getColumn :: Int -> IO [Text]
getColumn col = do
  bs <- L.readFile "Летальность.xlsx"
  let value r = case help of
                  Just (CellText t)   -> t
                  Just (CellDouble d) -> txtd d
                  _                   -> ""
                  where
                    help = toXlsx bs ^? ixSheet "Лист1"
                           . ixCell (r, col) . cellValue . _Just
  return $ [value r | r <- [2..4]] -- TODO ? sort

fio :: IO [Text]
fio = getColumn 2

number :: IO [Text]
number = getColumn 8

department :: IO [Text]
department = getColumn 7

getFIO :: Int -> IO Text
getFIO row = do
  bs <- L.readFile "Летальность.xlsx"
  let value = let Just (CellText t) = help
              in t
              where
                help = toXlsx bs ^? ixSheet "Лист1"
                       . ixCell (row, 2) . cellValue . _Just
  return value

getAll :: Int -> IO [Text]
getAll row = do
  bs <- L.readFile "Летальность.xlsx"
  let value c = case help of
                  Just (CellText t)   -> t
                  Just (CellDouble d) -> txtd d
                  _                   -> ""
                  where
                    help = toXlsx bs ^? ixSheet "Лист1"
                           . ixCell (row, c) . cellValue . _Just
  return $ [value c | c <- [2, 3, 7, 9]]
