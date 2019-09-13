{-# LANGUAGE OverloadedStrings #-}
module Excel.GogolSheet where
import           Network.Google
import           Network.Google.Resource.Sheets.Spreadsheets.Get
import           Network.Google.Sheets
import           Network.Google.Storage

import           Control.Lens                                    ((&), (.~),
                                                                  (<&>), (?~))
import           Data.Aeson.Types
import           Data.Text                                       (Text, splitOn)
import           System.IO                                       (stdout)

-- |
-- This gets the Information about an spreadsheet.
-- In order to be able to run these examples you need to
-- create a service acount from google's developers console
-- and copy the dowloaded json file to ~/.config/gcloud/application_default_credentials.json.
--
-- you must also share with your service the spreadsheet that you want to get the info of.
-- In order to do this you must share the sheet with the email address of your service
-- which is in your downloaded service config file.
--
-- after doing above step just pass the spreadsheet id to the function.
exampleGetSheet :: Text -> IO Spreadsheet
exampleGetSheet sheetID = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send (spreadsheetsGet sheetID )

-- |
-- you pass the sheet id and a range (eg. "sheet1!A1:C3") in that sheet
-- and it retreives the values in the specified range
exampleGetValue :: Text -> Text -> IO ValueRange
exampleGetValue sheetID range = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send  (spreadsheetsValuesGet sheetID range)


exampleAValue :: Text -> Text -> [[Value]] -> IO ()
exampleAValue sheetID range val =  do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send  (svaValueInputOption .~ Just "USER_ENTERED" $ spreadsheetsValuesAppend
          sheetID
          ( vrMajorDimension .~ Just VRMDRows $ vrValues .~ val $ vrRange .~ Just range $ valueRange)
          range )
  return ()

testId :: Text
testId = "1_KM9xgF-LpUsSrLTvxRhz7ZxEPrw_OGIWX6T-dAfNKM"
-- testId = "1d5s6kHQCsdU3xPDMesmEDS_H3XrcMd31vRNqphI3ebQ"
-- https://docs.google.com/spreadsheets/d/1d5s6kHQCsdU3xPDMesmEDS_H3XrcMd31vRNqphI3ebQ/edit#gid=1660848898
writeToGS :: Text -> IO ()
writeToGS link = do
  let sheetID = (splitOn "/" link) !! 5
  exampleAValue sheetID "A1:A" [[String "Ping", String "123"]]

--   -- TODO move secret json to ~./config/gcloud System.Directory
--   ss <- exampleGetValue testId "A1:A2"
--   exampleUpdateValue testId "A1:B2" [[String "Pong", String "Pong"], [String "Ping"]]
--   -- putStrLn $ show ss
--   return ()
