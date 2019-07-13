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
  void initGUI          -- (1)
  window <- windowNew   -- (2)
  set window [ windowTitle         := "ПаТаН"
             -- , windowResizable     := False
             , windowDefaultWidth  := 500
             , windowDefaultHeight := 100 ]
  display1 <- entryNew
  set display1 [
                 entryEditable := False
               , entryXalign   := 1 -- makes contents right-aligned
               , entryText     := "Номер по порядку"
               ]
  display1_1 <- entryNew
  -- widgetModifyText display1_1 StateNormal (Color 0 65535 0)
  set display1_1 [ entryPlaceholderText := Just "Ввод 1"
                 ]
  display2 <- entryNew
  set display2 [  entryEditable := False
                , entryXalign   := 0 -- makes contents right-aligned
                , entryText     := "И сюда" ]
  grid <- gridNew                  -- (1)
  gridSetRowHomogeneous grid True  -- (2)
  gridSetColumnHomogeneous grid True  -- (2)
  let attach x y w h item = gridAttach grid item x y w h -- (3)
  attach 0 0 1 1 display1           -- (4)
  attach 1 0 1 1 display1_1           -- (4)

  -- attach 0 1 1 1 display2           -- (4)
  -- mkBtn "MC"  >>= attach 0 1 1 1   -- (5)

  containerAdd window grid

  widgetShowAll window  -- (4)

  window `on` deleteEvent $ do -- handler to run on window destruction
    liftIO mainQuit
    return False
  display1_1 `on` entryActivated $ do
    test <- (entryGetText display1_1) :: IO String
    appendRTFStringOrPara out (Paragraph QCenter [RTFString Roman test])
    return ()
  mainGUI               -- (5)~
