{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Excel.Xman where
import           Codec.Xlsx
import           Codec.Xlsx.Writer.Internal (txtd)
import           Control.Lens
import qualified Data.ByteString.Lazy       as L
import           Data.Text                  (Text, pack)
import           Data.Time.Clock            (addUTCTime)
import           Data.Time.Clock.POSIX      (getPOSIXTime,
                                             posixSecondsToUTCTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)

test :: IO ()
test = do
  ct <- getPOSIXTime
  let
      sheet = def & cellValueAt (1,2) ?~ CellDouble 42.0
                  & cellValueAt (3,2) ?~ CellText "foo"
      xlsx = def & atSheet "List1" ?~ sheet
  L.writeFile "example.xlsx" $ fromXlsx ct xlsx


getMKB :: Int -> IO [Text]
getMKB col = do
  bs <- L.readFile "mkb10.xlsx"
  let value r = case help of
                  Just (CellText t) -> t
                  _                 -> ""
                  where
                    help = toXlsx bs ^? ixSheet "МКБ-10"
                           . ixCell (r, col) . cellValue . _Just
  return $ takeWhile (\t -> t /= "") [value r | r <- [5..]]

diagnosX :: IO [Text]
diagnosX = getMKB 2

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
  return $ takeWhile (\t -> t /= "") [value r | r <- [2..]] -- TODO ? sort

fioX, numberX, deptX :: IO [Text]
fioX = getColumn 2
numberX = getColumn 8
deptX = getColumn 7

getAll :: Int -> IO [Text]
getAll row = do
  bs <- L.readFile "Летальность.xlsx"
  let value c = case help of
                  Just (CellText t)   -> return t
                  Just (CellDouble d) ->
                    if (c == 9 || c == 17)
                    then do
                      let doubleToUTCTime = posixSecondsToUTCTime . realToFrac
                      let u = addUTCTime (-2209157400) (doubleToUTCTime $ (d * 86400)) -- because UTC from 1970, but Excel from 1900
                      let s = formatTime defaultTimeLocale "%m%d%_Y" u -- our format
                      return $ pack s
                    else return $ txtd d
                  _                   -> return ""
                  where
                    help = toXlsx bs ^? ixSheet "Лист1"
                           . ixCell (row, c) . cellValue . _Just
  sequence [value c | c <- [2, 3, 7, 4, 9, 17]]
