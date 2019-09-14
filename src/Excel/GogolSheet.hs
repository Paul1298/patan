{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Excel.GogolSheet where
import           Control.Lens                                    ((&), (.~),
                                                                  (<&>), (?~),
                                                                  (^.), (^..),
                                                                  (^?))
import           Control.Lens.Prism                              (_Just)
import           Control.Monad                                   (join, void)
import           Data.Aeson.Types                                hiding (Error)
import           Data.Text                                       (Text, breakOn,
                                                                  pack, splitOn)
import           Network.Google
import           Network.Google.Resource.Sheets.Spreadsheets.Get
import           Network.Google.Sheets                           hiding (Number)
import           System.IO                                       (stdout)

import           Excel.Xman                                      (entriesNumsToExcel)
import           Utils.Labels                                    (partList)


sendGoogleRequestToSheet :: Google
                            '["https://www.googleapis.com/auth/spreadsheets"] b
                            -> IO b
sendGoogleRequestToSheet request = do
  lgr <- newLogger Error stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $ request


getSheet :: Text -> IO Spreadsheet
getSheet sheetID = sendGoogleRequestToSheet $
  send (spreadsheetsGet sheetID)


getValue :: Text -> Text -> IO ValueRange
getValue sheetID range = sendGoogleRequestToSheet $
  send (spreadsheetsValuesGet sheetID range)


appendValue :: Text -> Text -> [[Value]] -> IO ()
appendValue sheetID range val = void $ sendGoogleRequestToSheet $
  send (svaValueInputOption .~ Just "USER_ENTERED" $ spreadsheetsValuesAppend
        sheetID
        (vrMajorDimension .~ Just VRMDRows $ vrValues .~ val $ vrRange .~ Just range $ valueRange)
        range)

updateValue :: Text -> Text -> [[Value]] -> IO ()
updateValue sheetID range val = void $ sendGoogleRequestToSheet $
  send (svuValueInputOption .~ Just "USER_ENTERED" $ spreadsheetsValuesUpdate
        sheetID
        (vrMajorDimension .~ Just VRMDRows $ vrValues .~ val $ vrRange .~ Just range $ valueRange)
        range)

-- border :: Text -> Text -> IO ()
-- border sheetID range = void $ sendGoogleRequestToSheet $
--   send  (updateBOrdersRequest )

writeToGS :: Text -> [Text] -> IO ()
writeToGS link answers = do
  let spsIDInLink = 5
      sheetID     = (splitOn "/" link) !! spsIDInLink

  spsheet <- getSheet sheetID
  let Just rowCount = (spsheet ^. sprSheets & head)
                       ^? sProperties
                       . _Just . sGridProperties
                       . _Just . gpRowCount
                       . _Just

  let values = (String . pack $ show rowCount) :
               map ($ undefined)
               (partList entriesNumsToExcel 0 answers
               (const . String . fst . breakOn " г."))

  appendValue sheetID "A1:A" [values]
