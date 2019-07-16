-- {-# LANGUAGE TemplateHaskell #-}
module GUI where
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Vector.Mutable         as M
import           Graphics.UI.Gtk             hiding (Action, backspace)
import           Graphics.UI.Gtk.Layout.Grid
import           System.IO

import           Text
import           Writer

startGUI :: Handle -> IO ()
startGUI out = do
  a <- M.new 7
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "ПаТаН"
             -- , windowDeletable     := False
             , windowDefaultWidth  := 500
             , windowDefaultHeight := 600 ]
  -- toArray
  fields <- sequence $ replicate 7 entryNew
  -- attrs = [ ent]
  sequence_ [set x [entryPlaceholderText := Just temp] | (x, temp) <- zip fields text] -- setup fields
  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True
  --
  -- sequence_ [gridAttach grid1 field 0 i 4 1 | (field, i) <- zip fields [0..6]]

  cal <- calendarNew -- calendar
  -- gridAttach grid1 cal 0 7 4 1

  tb <- checkButtonNewWithLabel "Готово"
  -- gridAttach grid1 tb 3 8 1 1

  -- ls <- listStoreNew ["asd", "dasd"]
  cb <- comboBoxNewText
  set cb [comboBoxTitle := "4. Пол:"]
  comboBoxPrependText cb $ T.pack "Мужской"
  comboBoxPrependText cb $ T.pack "Женский"
  -- comboBoxSetTitle cb "4. Пол:"
  comboBoxGetTitle cb >>= putStrLn
  gridAttach grid1 cb 0 9 1 1

  containerAdd window grid1



  widgetShowAll window

  -- signals section
  tb `on` toggled $ do
    liftIO mainQuit
    -- threadDelay 1000000 --sleep for a million microseconds, or one second

  let activ f = f `on` entryActivated $ do
              q <- entryGetPlaceholderText f
              a <- entryGetText f
              writeText1 out (fromMaybe "Fix me" q) a
  mapM activ fields

  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- start!
  mainGUI
