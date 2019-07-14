{-# LANGUAGE TemplateHaskell #-}
module GUI where
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Graphics.UI.Gtk             hiding (Action, backspace)
import           Graphics.UI.Gtk.Layout.Grid
import           System.IO

import           Writer

mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn

startGUI :: Handle -> IO ()
startGUI out = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "ПаТаН"
             , windowDeletable     := False
             , windowDefaultWidth  := 500
             , windowDefaultHeight := 100 ]
  display1 <- entryNew
  set display1 [ entryPlaceholderText := Just "Номер по порядку" ]
  display3 <- entryNew
  set display3 [ entryEditable := False
               , entryText := "Твоя пипирка" ]
  grid1 <- gridNew
  display2 <- entryNew
  set display2 [ entryPlaceholderText := Just "4. Пол:" ]
  grid2 <- gridNew
  -- gridSetRowHomogeneous grid True  -- (2)
  -- gridSetColumnHomogeneous grid True  -- (2)
  sb <- spinButtonNewWithRange 0 10 3


  gridAttach grid1 display1 0 0 1 1
  gridAttach grid1 display3 0 1 1 1
  gridAttach grid1 sb 2 1 1 1

  containerAdd window grid1

  widgetShowAll window

  window `on` deleteEvent $ do -- handler to run on window destruction
    liftIO mainQuit
    return False

  display1 `on` entryActivated $ do
    text <- (entryGetText display1) :: IO String
    appendRTFStringOrPara out (Paragraph QCenter [RTFString Roman text])
    containerRemove window grid1
    containerAdd window display2
    widgetShowAll window
    -- return ()
  display2 `on` entryActivated $ do
    text <- (entryGetText display2) :: IO String
    appendRTFStringOrPara out (Paragraph QLeft [RTFString Bold text])
    liftIO mainQuit
  mainGUI               -- (5)~
