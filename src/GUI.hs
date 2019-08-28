module GUI where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Graphics.UI.Gtk

import           CommonGUI
import           DefCombo
import           Fillings
import           Labels
import           Signals

startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle          := "ПаТаН"
             , containerBorderWidth := 8
             ]

  let n1 = length labels1
  (grid1, entries1, widgets1) <- initGrid n1 labels1 initDef1
  sign1sect grid1 entries1

  let n2 = length labels2 -- TODO delete using 0..

  exps <- sequence $ replicate n2 (expanderNew "")

  buts <- sequence [do
                      b <- buttonNewWithLabel l
                      expanderSetLabelWidget e b
                      return b | (e, l) <- zip exps labels2]

  ready <- buttonNewWithLabel "Сохранить документ"
  set ready [ widgetHExpand := True ]

  def2 <- defInner2
  (_, widgets2) <- unzip <$> sequence
            [ initGrid (length lab) lab (return def) >>= (\(g, e, w) -> containerAdd ex g >> return (e, w))
            | (ex, lab, def) <- zip3 exps labelsInner2 def2 ]
  fillings2 widgets2
  signSectChange ready widgets1 widgets2

  grid2 <- gridNew
  containerSetBorderWidth grid2 2
  gridSetRowSpacing grid2 2
  sequence_ [gridAttach grid2 e 0 i 2 1 | (e, i) <- zip exps [0..n2 - 1]]

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

  -- TODO need to speed up
  void $ window `on` configureEvent $ do
    (width, h) <- eventSize
    liftIO $ scrolledWindowSetMinContentHeight sw1 (h - 100)
    sequence_ [liftIO $ widgetSetSizeRequest b (width - 40) 10 | b <- buts]
    return False

  void $ window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна
  return ()
