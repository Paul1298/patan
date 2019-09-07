module GUI.GUI where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Graphics.UI.Gtk

import           GUI.CommonGUI
import           GUI.Fillings
import           GUI.Signals
import           Utils.DefCombo
import           Utils.Labels

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

  -- TODO add moving protocol id and doctor surname to default filePath
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
  -- containerAdd sw2 grid2

  box2 <- hBoxNew True 2
  choice1 <- buttonNewWithLabel "Заполнить по пунктам"
  frame <- frameNew
  frameSetLabel frame "Свободный ввод"
  frameSetLabelAlign frame 0.5 0.5
  containerAdd frame =<< textViewNew

  choice2 <- buttonNewWithLabel "Свободный ввод"
  containerAdd box2 choice1
  containerAdd box2 choice2
  void $ choice1 `on` buttonActivated $ do
    containerRemove sw2 box2
    containerAdd sw2 grid2
    widgetShowAll sw2

  containerAdd sw2 box2

  void $ choice2 `on` buttonActivated $ do
    containerRemove sw2 box2
    containerAdd sw2 frame
    widgetShowAll sw2

  void $ notebookAppendPage note sw2 "Макроскопическое исследование"

  grid <- gridNew
  gridAttach grid ready 0 0 1 1
  gridAttach grid note 0 1 1 1
  containerAdd window grid

  windowMaximize window
  widgetShowAll window
  notebookNextPage note

  -- TODO need to speed up
  void $ window `on` configureEvent $ do
    (width, h) <- eventSize
    liftIO $ scrolledWindowSetMinContentHeight sw1 (h - 100)
    sequence_ [liftIO $ widgetSetSizeRequest b (width - 40) 10 | b <- buts]
    return False

  void $ window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна
  return ()
