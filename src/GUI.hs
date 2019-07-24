module GUI where
-- import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text              (Text, pack)
import           Graphics.UI.Gtk
--hiding (Action, backspace)
import           System.IO
import           System.Process

import           DefCombo
import           Labels
import           Writer
import           Xman

getEntry :: ComboBox -> IO Entry
getEntry c = do
  Just w <- binGetChild c
  return (castToEntry w)

foo :: [Entry] -> TreeIter -> IO Bool
foo entries (TreeIter _ i _ _) = do
  putStrLn $ show (i + 2)
  getFIO (fromIntegral (i + 2) :: Int) >>= entrySetText (entries !! 3)
  return False

signals :: [Entry] -> [EntryCompletion] -> [ComboBox] -> CheckButton -> Window -> IO (ConnectId Window)
signals entries ecompls combos checkb window = do
  -- signals section
  let
    numberEC = ecompls !! 2
    numberCB = combos !! 2
    fioEC = ecompls !! 3

  numberEC `on` matchSelected $ (\tm ti -> foo entries ti)
  numberCB `on` changed $ do
    ti <- comboBoxGetActiveIter numberCB
    case ti of
      Just i -> foo entries i >> return ()
      _      -> return ()

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

  let n = length labels1

  -- labels
  labels <- sequence [labelNew $ Just l | l <- labels1] -- init labels
  sequence_ [miscSetAlignment l 0 0 | l <- labels] -- left alignment labels
  sequence_ [gridAttach grid1 l 0 i 1 1 | (l, i) <- zip labels [0..n - 1]] --attach them

  let textColumn = makeColumnIdString 0

  -- entries with comboBox
  def1 <- initDef1
  stores <- sequence [listStoreNew def | def <- def1]
  sequence_ [customStoreSetColumn store textColumn (\x -> pack x) | store <- stores ] -- set the extraction function

  -- combos
  combos <- sequence [comboBoxNewWithModelAndEntry store | store <- stores ]
  sequence_ [comboBoxSetEntryTextColumn combo textColumn | combo <- combos ] -- set which column should be used
  sequence_ [gridAttach grid1 field 1 i 1 1 | (field, i) <- zip combos [0..n - 1]]

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
  gridAttach grid1 checkb 1 (n + 1) 1 1

  containerAdd window grid1

  widgetShowAll window

  _ <- signals entries ecompls combos checkb window -- TODO add cbs
  return ()
