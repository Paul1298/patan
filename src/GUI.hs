module GUI where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Graphics.UI.Gtk
import           System.IO
import           System.Process

import           CommonGUI
import           DefCombo
import           Labels
import           Writer

startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle := "ПаТаН" ]
  (grid1, nextBut, entries1) <- initGrid 1 labels1 initDef1 "Макроскопическое исследование"
  (grid2, ready, entries2)   <- initGrid 2 labels2 initDef2 "Готово"

  containerAdd window grid1
  -- containerAdd window =<< test
  widgetShowAll window

  _ <- nextBut `on` buttonActivated $ do
    containerRemove window grid1
    containerAdd window grid2
    widgetShowAll window
    -- gridGetChildAt
  -- Готово
  _ <- ready `on` buttonActivated $ do
    out <- initRTF
    mapM entryGetText entries1 >>= writeText1 out
    mapM entryGetText entries2 >>= writeText2 out
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()

  -- windowMaximize window
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна
  return ()
