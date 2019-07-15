-- {-# LANGUAGE TemplateHaskell #-}
module GUI where
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Graphics.UI.Gtk             hiding (Action, backspace)
import           Graphics.UI.Gtk.Layout.Grid
import           System.IO

import           Text
import           Writer

-- cal <- calendarNew -- calendar
startGUI :: Handle -> IO ()
startGUI out = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "ПаТаН"
             -- , windowDeletable     := False
             , windowDefaultWidth  := 500
             , windowDefaultHeight := 100 ]
  fields <- sequence $ replicate 7 entryNew
  sequence_ [set x [entryPlaceholderText := Just temp] | (x, temp) <- zip fields text] -- setup fields
  grid1 <- gridNew
  -- set display2 [ entryPlaceholderText := Just "4. Пол:" ]
  -- grid2 <- gridNew
  gridSetRowHomogeneous grid1 True -- rows same height
  -- -- gridSetColumnHomogeneous grid True  -- (2)
  --
  sequence_ [gridAttach grid1 field 0 i 1 1 | (field, i) <- zip fields [0..6]]
  -- -- gridAttach grid1 display3 0 1 1 1
  -- gridAttach grid1 sb 5 1 5 5
  --
  containerAdd window grid1
  --

  --
  widgetShowAll window
  --
  -- grid1 `on` entryActivated $ do
  --   text <- (entryGetText display1) :: IO String
  --   containerRemove window grid1
  --   containerAdd window grid2
  --   widgetShowAll window
  --
  -- grid2 `on` entryActivated $ do
  --   text <- (entryGetText display2) :: IO String
  --   appendRTFStringOrPara out (Paragraph QLeft [RTFString Bold text])
  --   liftIO mainQuit
  window `on` deleteEvent $ liftIO mainQuit >> return False
  mainGUI
