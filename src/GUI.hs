module GUI where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Graphics.UI.Gtk        hiding (AlignCenter)
-- import           Graphics.UI.Gtk.General.Enums (Align (AlignCenter))
import           System.IO
import           System.Process

import           CommonGUI
import           DefCombo
import           Labels
import           Signals
import           Writer

startGUI :: IO ()
startGUI = do
  window <- windowNew --{ windowTitle = "ПаТаН" }
  set window [
               windowDefaultWidth  := 500
             , windowDefaultHeight := 500
             ]
  let n1 = length labels1
  (grid1, entries1) <- initGrid n1 labels1 initDef1

  butt1_2 <- buttonNewWithLabel "Клинические данные"
  gridAttach grid1 butt1_2 2 n1 1 1
  sign1sect grid1 entries1 butt1_2


  let n2 = length labels2
  grid2 <- gridNew

  -- widgetSetHAlign grid2 AlignCenter
  exps <- sequence $ replicate n2 (expanderNew "")
  sequence_ [buttonNewWithLabel l >>= expanderSetLabelWidget e | (e, l) <- zip exps labels2]
  def2 <- defInner2
  sequence_ [initGrid n2 lab (return def) >>= (\(g, _) -> containerAdd ex g) | (ex, lab, def) <- zip3 exps labelsInner2 def2 ]
  sequence_ [gridAttach grid2 e 0 i 1 1 | (e, i) <- zip exps [0..n2 - 1]] --attach them

  butt2_1 <- buttonNewWithLabel "Макроскопическое исследование"
  butt2_3 <- buttonNewWithLabel "Готово"

  -- containerGetChildren (exps !! 0) >>=  widgetGetName . last >>= putStrLn
  -- sp <- spinButtonNewWithRange 0 90 0.1
  -- gridAttach grid2 sp 0 n 1 1

  sw <- scrolledWindowNew Nothing Nothing
  containerAdd sw grid1
  containerAdd window sw
  widgetShowAll window

  signSectChange sw [grid1, grid2] [butt1_2, butt2_1, butt2_3] [entries1]

  -- windowMaximize window
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна
  return ()
