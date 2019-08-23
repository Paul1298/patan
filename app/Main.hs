module Main where
import           Graphics.UI.Gtk

import           GUI

main :: IO ()
main = do
  _ <- initGUI
  startGUI
  mainGUI
