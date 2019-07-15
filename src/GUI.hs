-- {-# LANGUAGE TemplateHaskell #-}
module GUI where
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe                  (fromMaybe)
import           Graphics.UI.Gtk             hiding (Action, backspace)
import           Graphics.UI.Gtk.Layout.Grid
import           System.IO

import           Text
import           Writer

startGUI :: Handle -> IO ()
startGUI out = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "ПаТаН"
             -- , windowDeletable     := False
             , windowDefaultWidth  := 500
             , windowDefaultHeight := 600 ]
  fields <- sequence $ replicate 7 entryNew
  -- attrs = [ ent]
  sequence_ [set x [entryPlaceholderText := Just temp] | (x, temp) <- zip fields text] -- setup fields
  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True
  --
  sequence_ [gridAttach grid1 field 0 i 1 1 | (field, i) <- zip fields [0..6]]

  cal <- calendarNew -- calendar
  gridAttach grid1 cal 0 7 1 1

  containerAdd window grid1
  --
  --
  widgetShowAll window

  -- signals section
  let activ f = f `on` entryActivated $ do
              q <- entryGetPlaceholderText f
              a <- entryGetText f
              writeText1 out (fromMaybe "Fix me" q) a
  mapM activ fields

  let quitEnt = last fields
  quitEnt `on` entryActivated $ liftIO mainQuit

  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- start!
  mainGUI
