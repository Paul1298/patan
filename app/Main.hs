module Main where
import           Control.Monad   (void)
import           Graphics.UI.Gtk

import           Excel.Xman
import           GUI.GUI

main :: IO ()
main = do
  -- test
  void initGUI
  startGUI
  mainGUI
