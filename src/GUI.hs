module GUI where

import           Control.Monad.IO.Class
import           Graphics.UI.Gtk        hiding (AlignCenter)
-- import           Graphics.UI.Gtk.General.Enums (Align (AlignCenter))

import           CommonGUI
import           DefCombo
import           Labels
import           Signals

startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle         := "ПаТаН"
             , windowDefaultWidth  := 820
             , windowDefaultHeight := 1080
             ]
  let n1 = length labels1
  (grid1, entries1) <- initGrid n1 labels1 initDef1
  sign1sect grid1 entries1

  let n2 = length labels2
  grid2 <- gridNew
  gridSetColumnHomogeneous grid2 True

  -- widgetSetHAlign grid2 AlignCenter
  exps <- sequence $ replicate n2 (expanderNew "")
  sequence_ [do
            b <- buttonNewWithLabel l
            widgetSetSizeRequest b 300 5
            -- set b [ widgetHExpandSet := True ]
            -- b `on` focusInEvent $ tryEvent $ do
            --   (width, height) <- eventSize
            --   putStrLn (show width ++ " x " ++ show height)
            --   return False
            expanderSetLabelWidget e b | (e, l) <- zip exps labels2]
  -- b <- expanderGetLabelWidget (exps !! 0)
  -- buttonSetAlignment (castToButton b) (0.5, 0.0)
  -- expanderGetLabelWidget (exps !! 0) >>= buttonGetAlignment . castToButton >>= putStrLn . show
  def2 <- defInner2
  sequence_ [initGrid (length lab) lab (return def) >>= (\(g, _) -> containerAdd ex g) | (ex, lab, def) <- zip3 exps labelsInner2 def2 ]
  sequence_ [gridAttach grid2 e 0 i 2 1 | (e, i) <- zip exps [0..n2 - 1]] --attach them


  note <- notebookNew
  notebookAppendPage note grid1 "Клинические данные"

  sw <- scrolledWindowNew Nothing Nothing
  containerAdd sw grid2
  notebookAppendPage note sw "Макроскопическое исследование"
  containerAdd window note

  widgetShowAll window

  -- windowMaximize window
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна
  return ()
