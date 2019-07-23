module GUI where
-- import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List              (isPrefixOf)
import           Data.Text              (pack, unpack)
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

textColumn = makeColumnIdString 0

signals :: [Entry] -> CheckButton -> Window -> IO (ConnectId Window)
signals entries checkb window = do
  -- signals section

  -- Готово
  _ <- checkb `on` toggled $ do
    out <- initRTF
    mapM entryGetText entries >>= writeText1 out
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()

  -- Закрытие окна
  window `on` deleteEvent $ liftIO mainQuit >> return False

startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle          := "ПаТаН" ]
  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True

  let n = length text1

  -- labels
  labels <- sequence [labelNew $ Just t | t <- text1] -- init labels
  sequence_ [miscSetAlignment l 0 0 | l <- labels] -- left alignment labels
  sequence_ [gridAttach grid1 l 0 i 3 1 | (l, i) <- zip labels [0..n - 1]] --attach them

  -- entries with comboBox
  stores <- sequence [listStoreNew def | def <- def1]
  sequence_ [customStoreSetColumn store textColumn (\x -> pack x) | store <- stores ] -- set the extraction function

  -- combos
  combos <- sequence [comboBoxNewWithModelAndEntry store | store <- stores ]
  sequence_ [comboBoxSetEntryTextColumn combo textColumn | combo <- combos ] -- set which column should be used
  sequence_ [gridAttach grid1 field 3 i 1 1 | (field, i) <- zip combos [0..n - 1]]

  -- entries
  entries <- mapM getEntry combos

  -- entry-completion
  ecompls <- sequence $ replicate n entryCompletionNew
  sequence_ [set ec [ entryCompletionModel            := Just st
                    , entryCompletionMinimumKeyLength := 0
                    , entryCompletionTextColumn       := textColumn] | (ec, st) <- zip ecompls stores]

  -- sequence_ [entryCompletionSetMinimumKeyLength ec 0 | ec <- ecompls]
  sequence_ [entrySetCompletion e ec | (e, ec) <- zip entries ecompls]

  checkb <- checkButtonNewWithLabel "Готово"
  gridAttach grid1 checkb 3 (n + 1) 1 1

  -- cal <- calendarNew -- calendar
  -- gridAttach grid1 cal 0 n 1 1

  containerAdd window grid1

  widgetShowAll window

  _ <- signals entries checkb window
  return ()
