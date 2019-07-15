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

startGUI :: Handle -> IO ()
startGUI out = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "ПаТаН"
             -- , windowDeletable     := False
             , windowDefaultWidth  := 500
             , windowDefaultHeight := 100 ]
  field1 <- entryNew
  let fields = replicate 7 field1
  -- putStrLn $ show $ zip text text
  sequence_ [set x [entryPlaceholderText := Just temp] | (x, temp) <- zip fields text] -- setup fields
  grid1 <- gridNew
  cal <- calendarNew
  -- set display2 [ entryPlaceholderText := Just "4. Пол:" ]
  -- grid2 <- gridNew
  -- -- gridSetRowHomogeneous grid True  -- (2)
  -- -- gridSetColumnHomogeneous grid True  -- (2)
  --
  -- gridAttach grid1 display1 0 0 1 1
  -- -- gridAttach grid1 display3 0 1 1 1
  -- gridAttach grid1 sb 5 1 5 5
  --
  -- containerAdd window (fields !! 5)
  containerAdd window cal
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
