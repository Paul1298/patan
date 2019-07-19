module GUI where
-- import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text              as T
import qualified Data.Vector            as V
--(freeze, fromList, toList)
import qualified Data.Vector.Mutable    as M
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

signals :: Int -> V.Vector Entry -> CheckButton -> Window -> IO (ConnectId Window)
signals n fields checkb window = do
  -- signals section
  replicate
  rep
  mv <- M.replicate n ""

  -- Enrties
  let activ f = f `on` entryActivated $ do --use this!
                a <- entryGetText f :: IO String
                let (Just i) = f `V.elemIndex` fields
                M.write mv i a
  mapM_ activ fields

  -- -- test
  -- (fields V.! 0) `on` editableChanged $ do
  --   putStrLn "Ай"
    -- return False
    -- st <- widgetGetState (fields V.! 0)
    -- foo st

  -- Готово
  _ <- checkb `on` toggled $ do
    out <- initRTF
    -- buttonSetLabel tb "Не нажимать"
    tmp <- V.freeze mv
    writeText1 out (V.toList tmp)
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()

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
  -- fields <- sequence . V.fromList $ replicate n entryNew

  let textColumn = makeColumnIdString 0

  stores <- sequence [listStoreNew def | def <- def1]
  sequence_ [customStoreSetColumn store textColumn (\x -> T.pack x) | store <- stores ] -- set the extraction function

  combos <- sequence [comboBoxNewWithModelAndEntry store | store <- stores ]
  sequence_ [comboBoxSetEntryTextColumn combo textColumn | combo <- combos ] -- set which column should be used
  sequence_ [gridAttach grid1 field 3 i 1 1 | (field, i) <- zip combos [0..n - 1]]

  ens <- mapM containerGetChildren combos
  let fields = V.fromList $ (map (castToEntry . head) ens)

  checkb <- checkButtonNewWithLabel "Готово"
  gridAttach grid1 checkb 3 (n + 1) 1 1

  -- ls <- listStoreNew ["ыв", "авы", "ыва"]
  -- cb <- comboBoxNewWithModel ls

  -- comboBoxPrependText cb $ T.pack "Мужской"
  -- comboBoxPrependText cb $ T.pack "Женский"
  -- comboBoxSetActive cb 0
  -- comboBoxGetTitle cb >>= putStrLn
  -- gridAttach grid1 cb 3 3 1 1
  --
  -- cls <- comboBoxSetModelText cb >>= listStoreToList

  -- cal <- calendarNew -- calendar
  -- gridAttach grid1 cal 0 n 1 1


  -- combo `on` entryActivated $ putStrLn "Ай"
  containerAdd window grid1
  widgetShowAll window

  _ <- signals n fields checkb window
  -- start!
  mainGUI
