module GUI where
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List                   (elemIndex, maximumBy)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Data.Vector                 (freeze, toList)
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
  out <- initRTF
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "ПаТаН"
             , windowDefaultWidth  := 1000 ]
             -- , windowDefaultHeight := 600 ]
  -- putStrLn $ show . length $ maximumBy (\a b -> if length a > length b then GT else LT) text1
  -- toArray
  let n = length text1

  temps <- sequence $ replicate n entryNew
  fields <- sequence $ replicate n entryNew
  -- attrs = [ ent]
  sequence_ [set x [ entryEditable := False, entryText := temp] | (x, temp) <- zip temps text1] -- setup fields
  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True
  cb <- comboBoxNewText
  set cb [comboBoxTitle := "Пол"]
  -- comboBoxSetTitle cb "asd"
  comboBoxPrependText cb $ T.pack "Мужской"
  comboBoxPrependText cb $ T.pack "Женский"
  -- comboBoxSetActive cb 0
  -- comboBoxGetTitle cb >>= putStrLn
  gridAttach grid1 cb 3 3 1 1
  --
  sequence_ [gridAttach grid1 temp 0 i 3 1 | (temp, i) <- zip temps [0..n - 1]]
  sequence_ [gridAttach grid1 field 3 i 1 1 | (field, i) <- zip fields [0..n - 1]]

  -- cal <- calendarNew -- calendar
  -- gridAttach grid1 cal 0 n 1 1

  tb <- checkButtonNewWithLabel "Готово"
  gridAttach grid1 tb 3 (n + 1) 1 1

  -- exitb <- checkButtonNewWithLabel "Выход"
  -- gridAttach grid1 tb 3 9 1 1


  containerAdd window grid1

  widgetShowAll window

  mv <- M.replicate n ""
  -- signals section

  -- Enrties
  let activ f = f `on` entryActivated $ do
                a <- entryGetText f :: IO String
                let (Just i) = f `elemIndex` fields
                M.write mv i a
  mapM activ fields

  -- Готово
  tb `on` toggled $ do
    buttonSetLabel tb "Не нажимать"
    tmp <- freeze mv
    writeText1 out (toList tmp)
    endRTF out
    hClose out
    r <- createProcess (proc "loffice" [pathFile]) --linux
    -- pid <- runCommand ("start " ++ pathFile) --win
    -- r <- createProcess (proc "start" [pathFile]) { create_new_console = True } -- win
    return ()

  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- start!
  mainGUI
