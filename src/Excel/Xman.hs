{-# LANGUAGE OverloadedStrings #-}

module Excel.Xman where
import           Codec.Xlsx
import           Codec.Xlsx.Writer.Internal (txtd)
import           Control.Lens
import           Control.Monad              ((<$!>))
import qualified Data.ByteString.Lazy       as L
-- import           Data.ByteString.Lazy.UTF8  as BLU
import           Data.Text                  (Text, pack)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           System.IO

import           Utils.Labels

entriesNumsToExcel :: [Int]
entriesNumsToExcel =
  [
    medRecLabNum
  , fioLabNum
  , depLabNum
  , sexLabNum
  , ageLabNum
  , dateDeathLabNum
  , badDayLabNum
  , datePsyLabNum
  ]

firstSheet :: Xlsx -> Worksheet
firstSheet xlsx = snd . head $ xlsx ^. xlSheets

firstSheetName :: Xlsx -> Text
firstSheetName xlsx = fst . head $ xlsx ^. xlSheets

writeToEx :: String -> IO ()
writeToEx fileName = do
  ct <- getPOSIXTime
  file <- openBinaryFile fileName ReadWriteMode
  xlsx_old <- toXlsx <$!> L.hGetContents file
  hClose file
  let
      sheet = firstSheet xlsx_old & cellValueAt (1,2) ?~ CellDouble 42.0
                                  & cellValueAt (1,1) ?~ CellText "Тест1"
      xlsx  = xlsx_old & atSheet (firstSheetName xlsx_old) ?~ sheet
  L.writeFile fileName $ fromXlsx ct xlsx
  -- L.writeFile fileName $ fromXlsx ct xlsx_old

getMKB :: Int -> IO [Text]
getMKB col = do
  xlsx <- toXlsx <$> L.readFile "resources/mkb10.xlsx"
  let value r = case help of
                  Just (CellText t) -> t
                  _                 -> ""
                  where
                    help = firstSheet xlsx ^?
                           ixCell (r, col) . cellValue . _Just
  return $ takeWhile (\t -> t /= "") [value r | r <- [5..]]

diagnosX :: IO [Text]
diagnosX = getMKB 2

getColumn :: String -> Int -> IO [Text]
getColumn fileName col = do
  xlsx <- toXlsx <$> L.readFile fileName
  let value r = case help of
                  Just (CellText t)   -> t
                  Just (CellDouble d) -> txtd d
                  _                   -> ""
                  where
                    help = firstSheet xlsx ^?
                           ixCell (r, col) . cellValue . _Just
  return $ takeWhile (\t -> t /= "") [value r | r <- [2..]] -- TODO ? sort

fioX, numberX, deptX :: IO [Text]
fioX = getColumn "resources/Летальность.xlsx" 2
numberX = getColumn "resources/Летальность.xlsx" 8
deptX = getColumn "resources/Летальность.xlsx" 7

getAll :: Int -> IO [Text]
getAll row = do
  xlsx <- toXlsx <$> L.readFile "resources/Летальность.xlsx"
  let value c = case help of
                  Just (CellText t)   -> return t
                  Just (CellDouble d) ->
                    if (c == 9 || c == 17)
                    then do
                      let u = dateFromNumber (xlsx ^. xlDateBase) d
                          s = formatTime defaultTimeLocale "%d%m%_Y" u
                      return $ pack s
                    else return $ txtd d
                  _                   -> return ""
                  where
                    help = firstSheet xlsx ^?
                           ixCell (row, c) . cellValue . _Just
  sequence [value c | c <- [2, 3, 7, 4, 9, 17]]
