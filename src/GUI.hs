module GUI where
import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Tree
-- import           Data.List                   (elemIndex, maximumBy)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
--(freeze, fromList, toList)
import qualified Data.Vector.Mutable         as M
import           Graphics.UI.Gtk
--hiding (Action, backspace)
import           Graphics.UI.Gtk.Layout.Grid
import           System.IO
import           System.Process

import           Text
import           Writer

initRTF :: IO Handle
initRTF = do
  te <- mkTextEncoding "CP1251"
  out <- openFile pathFile WriteMode
  hSetEncoding out te
  startRTF out
  return out



startGUI :: IO ()
startGUI = do
  initGUI
  window <- windowNew
  set window [ windowTitle          := "ПаТаН"
             -- , windowWindowPosition := WinPosCenterOnParent
             , windowDefaultWidth   := 580 ]
             -- , windowDefaultHeight := 600 ]
  -- putStrLn $ show . length $ maximumBy (\a b -> if length a > length b then GT else LT) text1
  -- toArray
  let n = length text1

  temps <- sequence [labelNew $ Just t | t <- text1]
  fields <- sequence . V.fromList $ replicate n entryNew
  sequence_ [miscSetAlignment t 0 0 | t <- temps]

  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True

  -- ls <- listStoreNew ["ыв", "авы", "ыва"]
  -- cb <- comboBoxNewWithModel ls

  -- comboBoxPrependText cb $ T.pack "Мужской"
  -- comboBoxPrependText cb $ T.pack "Женский"
  -- comboBoxSetActive cb 0
  -- comboBoxGetTitle cb >>= putStrLn
  -- gridAttach grid1 cb 3 3 1 1
  --
  -- cls <- comboBoxSetModelText cb >>= listStoreToList

  sequence_ [gridAttach grid1 temp 0 i 3 1 | (temp, i) <- zip temps [0..n - 1]]
  sequence_ [gridAttach grid1 field 3 i 1 1 | (field, i) <- zip (V.toList fields) [0..n - 1]]

  -- cal <- calendarNew -- calendar
  -- gridAttach grid1 cal 0 n 1 1

  tb <- checkButtonNewWithLabel "Готово"
  gridAttach grid1 tb 3 (n + 1) 1 1


    -- Pressing Alt+H will activate this button
  -- label <- labelNew $ Just "This"

  containerAdd window grid1
  -- containerAdd window label
  widgetShowAll window

  mv <- M.replicate n ""

  -- signals section

  -- Enrties
  let activ f = f `on` entryActivated $ do --use this!
                a <- entryGetText f :: IO String
                let (Just i) = f `V.elemIndex` fields
                M.write mv i a
  mapM activ fields

  -- test
  (fields V.! 0) `on` editableChanged $ do
    putStrLn "Ай"
    -- return False
    -- st <- widgetGetState (fields V.! 0)
    -- foo st

  -- Готово
  tb `on` toggled $ do
    out <- initRTF
    buttonSetLabel tb "Не нажимать"
    tmp <- V.freeze mv
    writeText1 out (V.toList tmp)
    endRTF out
    hClose out
    r <- createProcess (proc "loffice" [pathFile]) --linux
    -- pid <- runCommand ("start " ++ pathFile) --win
    return ()

  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- start!
  mainGUI
