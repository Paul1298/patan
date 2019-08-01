{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Xman where
import           Codec.Xlsx
import           Codec.Xlsx.Writer.Internal (txtd)
import           Control.Lens
import qualified Data.ByteString.Lazy       as L
import           Data.Text                  (Text, pack)
import           Data.Time.Clock            (addUTCTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)

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

fioX, numberX, deptX :: IO [Text]
fioX = getColumn 2
numberX = getColumn 8
deptX = getColumn 7

getAll :: Int -> IO [Text]
getAll row = do
  bs <- L.readFile "Летальность.xlsx"
  let value c = case help of
                  Just (CellText t)   -> return t
                  Just (CellDouble d) -> do
                    let doubleToUTCTime = posixSecondsToUTCTime . realToFrac
                    let u = addUTCTime (-2209157400) (doubleToUTCTime $ (d * 86400)) -- because UTC from 1970, but Excel from 1900
                    let s = formatTime defaultTimeLocale "%m.%d.%_Y" u -- our format
                    return $ pack s
                  _                   -> return ""
                  where
                    help = toXlsx bs ^? ixSheet "Лист1"
                           . ixCell (row, c) . cellValue . _Just
  sequence [value c | c <- [2, 3, 7, 9]]
