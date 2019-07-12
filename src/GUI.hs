{-# LANGUAGE TemplateHaskell #-}
module GUI where
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Graphics.UI.Gtk             hiding (Action, backspace)
import           Graphics.UI.Gtk.Layout.Grid

mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn

startGUI :: IO ()
startGUI = do
  void initGUI          -- (1)
  window <- windowNew   -- (2)
  set window [ windowTitle         := "ПаТаН"
             -- , windowResizable     := False
             , windowDefaultWidth  := 500
             , windowDefaultHeight := 500 ]
  display1 <- entryNew
  set display1 [  entryEditable := False
                , entryXalign   := 1 -- makes contents right-aligned
                , entryText     := "Введите сюда что-нибудь, пожалуйста" ]
  display1_1 <- entryNew
  widgetModifyText display1_1 StateNormal (Color 0 65535 0)
  set display1_1 [ entryText     := "Ввод 1" ]
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
  attach 0 1 1 1 display2           -- (4)
  -- mkBtn "MC"  >>= attach 0 1 1 1   -- (5)
  -- mkBtn "MR"  >>= attach 1 1 1 1
  -- mkBtn "MS"  >>= attach 2 1 1 1
  -- mkBtn "M+"  >>= attach 3 1 1 1
  -- mkBtn "M–"  >>= attach 4 1 1 1
  -- mkBtn "←"   >>= attach 0 2 1 1
  -- mkBtn "CE"  >>= attach 1 2 1 1
  -- mkBtn "C"   >>= attach 2 2 1 1
  -- mkBtn "±"   >>= attach 3 2 1 1
  -- mkBtn "√"   >>= attach 4 2 1 1
  -- mkBtn "7"   >>= attach 0 3 1 1
  -- mkBtn "8"   >>= attach 1 3 1 1
  -- mkBtn "9"   >>= attach 2 3 1 1
  -- mkBtn "÷"   >>= attach 3 3 1 1
  -- mkBtn "%"   >>= attach 4 3 1 1
  -- mkBtn "4"   >>= attach 0 4 1 1
  -- mkBtn "5"   >>= attach 1 4 1 1
  -- mkBtn "6"   >>= attach 2 4 1 1
  -- mkBtn "*"   >>= attach 3 4 1 1
  -- mkBtn "1/x" >>= attach 4 4 1 1
  -- mkBtn "1"   >>= attach 0 5 1 1
  -- mkBtn "2"   >>= attach 1 5 1 1
  -- mkBtn "3"   >>= attach 2 5 1 1
  -- mkBtn "–"   >>= attach 3 5 1 1
  -- mkBtn "="   >>= attach 4 5 1 2
  -- mkBtn "0"   >>= attach 0 6 2 1
  -- mkBtn "."   >>= attach 2 6 1 1
  -- mkBtn "+"   >>= attach 3 6 1 1

  containerAdd window grid         -- (6)
  window `on` deleteEvent $ do -- handler to run on window destruction
    liftIO mainQuit
    return False

  widgetShowAll window  -- (4)
  mainGUI               -- (5)
