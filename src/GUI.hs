module GUI where
-- import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List              (isPrefixOf)
import           Data.Text              (pack)
import           Graphics.UI.Gtk
--hiding (Action, backspace)
import           System.IO
import           System.Process

import           DefCombo
import           Text
import           Writer

initRTF :: IO Handle
initRTF = do
  te <- mkTextEncoding "CP1251"
  out <- openFile pathFile WriteMode
  hSetEncoding out te
  startRTF out
  return out

getEntry :: ComboBox -> IO Entry
getEntry c = do
  Just w <- binGetChild c
  return (castToEntry w)
--

s = def1 !! 9

textColumn = makeColumnIdString 0

-- test c = do
--    f <- getEntry c
--    _ <- f `on` editableChanged $ do
--      t <- entryGetText f
--      let tmp = filter (isPrefixOf t) s
--      -- putStrLn tmp
--      if (not $ null tmp)
--        then
--          do
--          customStoreSetColumn store textColumn (\x -> pack x)
--
--        -- combos <- sequence [comboBoxNewWithModelAndEntry store | store <- stores ]
--          comboBoxSetModel c (Just store)
--          -- comboBoxPopup c
--        else
--          return ()
--    return ()


signals :: [ComboBox] -> CheckButton -> Window -> IO (ConnectId Window)
signals combos checkb window = do
  fields <- mapM getEntry combos
  -- signals section

  -- Autocomplete
  let auto c = c `on` changed $ do
        -- test c
        undefined
    -- f <- containerGetChildren c
    -- f `on` editableChanged $ do
    -- -- entryGetText f >>= putStrLn
        -- undefined

  mapM_ auto combos

  -- Готово
  _ <- checkb `on` toggled $ do
    out <- initRTF
    mapM entryGetText fields >>= writeText1 out
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()

  -- Закрытие окна
  window `on` deleteEvent $ liftIO mainQuit >> return False

startGUI :: IO ()
startGUI = do
  _ <- initGUI
  window <- windowNew
  set window [ windowTitle          := "ПаТаН" ]
  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True

  let n = length text1

  -- labels
  labels <- sequence [labelNew $ Just t | t <- text1] -- init labels
  sequence_ [miscSetAlignment l 0 0 | l <- labels] -- left alignment labels
  sequence_ [gridAttach grid1 l 0 i 3 1 | (l, i) <- zip labels [0..n - 1]] -- attach them

  -- entries with comboBox
  stores <- sequence [listStoreNew def | def <- def1]
  sequence_ [customStoreSetColumn store textColumn (\x -> pack x) | store <- stores ] -- set the extraction function

  combos <- sequence [comboBoxNewWithModelAndEntry store | store <- stores ]
  sequence_ [comboBoxSetEntryTextColumn combo textColumn | combo <- combos ] -- set which column should be used
  sequence_ [gridAttach grid1 field 3 i 1 1 | (field, i) <- zip combos [0..n - 1]]

  checkb <- checkButtonNewWithLabel "Готово"
  gridAttach grid1 checkb 3 (n + 1) 1 1

  -- cal <- calendarNew -- calendar
  -- gridAttach grid1 cal 0 n 1 1

  q <- entryCompletionNew
  store <- listStoreNew s
  customStoreSetColumn store textColumn (\x -> pack x)
  entryCompletionSetModel q $ Just store
  entryCompletionSetTextColumn q textColumn
  e <- entryNew
  entrySetCompletion e q
  gridAttach grid1 e 0 (n + 1) 1 1

  containerAdd window grid1
  widgetShowAll window

  _ <- signals combos checkb window
  -- start!
  mainGUI
