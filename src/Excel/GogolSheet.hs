{-# LANGUAGE OverloadedStrings #-}
module Excel.GogolSheet where
import           Network.Google
import           Network.Google.Resource.Sheets.Spreadsheets.Get
import           Network.Google.Sheets                           hiding (Number)
import           Network.Google.Storage

import           Control.Lens                                    ((&), (.~),
                                                                  (<&>), (?~),
                                                                  (^.), (^?))
import           Control.Lens.Prism                              (_Just)
import           Data.Aeson.Types                                hiding (Error)
import           Data.Text                                       (Text, breakOn,
                                                                  pack, splitOn)
import           System.IO                                       (stdout)

import           Excel.Xman                                      (entriesNumsToExcel)
import           Utils.Labels                                    (partList)


exampleGetSheet :: Text -> IO Spreadsheet
exampleGetSheet sheetID = do
  lgr <- newLogger Error stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send (spreadsheetsGet sheetID )


exampleGetValue :: Text -> Text -> IO ValueRange
exampleGetValue sheetID range = do
  lgr <- newLogger Error stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send  (spreadsheetsValuesGet sheetID range)


exampleAValue :: Text -> Text -> [[Value]] -> IO ()
exampleAValue sheetID range val =  do
  lgr <- newLogger Error stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send  (svaValueInputOption .~ Just "USER_ENTERED" $ spreadsheetsValuesAppend
          sheetID
          ( vrMajorDimension .~ Just VRMDRows $ vrValues .~ val $ vrRange .~ Just range $ valueRange)
          range )
  return ()

exampleUValue :: Text -> Text -> [[Value]] -> IO ()
exampleUValue sheetID range val =  do
  lgr <- newLogger Error stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send  (svuValueInputOption .~ Just "USER_ENTERED" $ spreadsheetsValuesUpdate
          sheetID
          (vrMajorDimension .~ Just VRMDRows $ vrValues .~ val $ vrRange .~ Just range $ valueRange)
          range)
  return ()

-- border :: Text -> Text -> [[Value]] ->  IO ()
-- border sheetID range val =  do
--   lgr <- newLogger Error stdout
--   env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
--   runResourceT . runGoogle env $
--     send  (updateBOrdersRequest .~ Just "USER_ENTERED" $ spreadsheetsValuesUpdate
--           sheetID
--           ( vrMajorDimension .~ Just VRMDRows $ vrValues .~ val $ vrRange .~ Just range $ valueRange)
--           range )
--   return ()

writeToGS :: Text -> [Text] -> IO ()
writeToGS link answers = do
  let spsIDInLink = 5
      sheetID     = (splitOn "/" link) !! spsIDInLink

  spreadsheet <- exampleGetSheet sheetID
  let Just rowCount = (spreadsheet ^. sprSheets & head)
                       ^. sProperties
                       >>= (flip (^.)) sGridProperties -- . _Just
                       -- ^. sGridProperties . _Just
                       >>= (flip (^.)) gpRowCount

  let values = (String . pack $ show rowCount) :
               map ($ undefined)
               (partList entriesNumsToExcel 0 answers
               (const . String . fst . breakOn " г."))

  -- putStrLn $ show rowCount

  -- exampleGetValue link ""
  exampleAValue sheetID "A1:A" [values]
  -- exampleUValue sheetID "B5:5" [values]

--   -- TODO move secret json to ~./config/gcloud System.Directory
--   ss <- exampleGetValue testId "A1:A2"
--   exampleUpdateValue testId "A1:B2" [[String "Pong", String "Pong"], [String "Ping"]]
--   -- putStrLn $ show ss
--   return ()
