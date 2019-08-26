{-# LANGUAGE ScopedTypeVariables #-}
module GUI where

import           CommonGUI
import           Control.Monad          (void)
import           Control.Monad.Extra    (fromMaybeM)
import           Control.Monad.IO.Class (liftIO)
import           DefCombo
import           Graphics.UI.Gtk
import           Labels
import           Signals

startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle          := "ПаТаН"
             -- , windowAllowGrow      := False -- TODO расширение на винде
             -- , windowDefaultWidth   := 820
             -- , windowDefaultHeight  := 1080
             , containerBorderWidth := 8
             ]
  -- let n0 = length labels0


  let n1 = length labels1
  (grid1, entries1) <- initGrid n1 labels1 initDef1
  sign1sect grid1 entries1

  let n2 = length labels2 -- TODO delete using 0..

  exps <- sequence $ replicate n2 (expanderNew "")

  buts <- sequence [do
                      b <- buttonNewWithLabel l
                      expanderSetLabelWidget e b
                      return b | (e, l) <- zip exps labels2]

  ready <- buttonNewWithLabel "Сохранить документ"
  set ready [ widgetHExpand := True ]

  void $ window `on` configureEvent $ do
    (width, _) <- eventSize
    sequence_ [liftIO $ widgetSetSizeRequest b (width - 40) 10 | b <- buts]
    return False

  def2 <- defInner2
  entries2 <- sequence
            [ initGrid (length lab) lab (return def) >>= (\(g, e) -> containerAdd ex g >> return e)
            | (ex, lab, def) <- zip3 exps labelsInner2 def2 ]

  widgets1 <- sequence [fromMaybeM undefined $ gridGetChildAt grid1 1 i | i <- [0..n1 - 1]]
  signSectChange ready widgets1 entries2
  grid2 <- gridNew
  containerSetBorderWidth grid2 2
  gridSetRowSpacing grid2 2
  sequence_ [gridAttach grid2 e 0 i 2 1 | (e, i) <- zip exps [0..n2 - 1]] --attach them


  note <- notebookNew
  sw1 <- scrolledWindowNew Nothing Nothing
  containerAdd sw1 grid1
  void $ notebookAppendPage note sw1 "Клинические данные"

  sw2 <- scrolledWindowNew Nothing Nothing
  containerAdd sw2 grid2
  void $ notebookAppendPage note sw2 "Макроскопическое исследование"

  grid <- gridNew
  gridAttach grid ready 0 0 1 1
  gridAttach grid note 0 1 1 1
  containerAdd window grid

  windowMaximize window
  widgetShowAll window
  -- let tf i = do
  --            tv <- frameNew
  --            containerAdd tv =<< textViewNew
  --            widgetSizeAllocate tv =<< (widgetGetAllocation $ entries1 !! (i - 1))
  --            containerRemove grid1 =<< fromMaybeM undefined (widgetGetParent $ entries1 !! i)
  --            gridAttach grid1 tv 1 i 1 1
  --            widgetShowAll tv
  -- mapM_ tf [23, 24, 25]
  scrolledWindowSetMinContentHeight sw1 . (+10) =<< widgetGetAllocatedHeight grid1

  void $ window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна
  return ()
