module GUI.GUI where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Graphics.UI.Gtk

import           GUI.CommonGUI
import           GUI.Fillings
import           GUI.Signals
import           Utils.DefCombo
import           Utils.Labels


clinicalData :: IO (ScrolledWindow, [Widget])
clinicalData = do
  let n1 = length labels1
  (grid1, entries1, widgets1) <- initGrid n1 labels1 initDef1
  signalsClinicalData grid1 entries1

  sw1 <- scrolledWindowNew Nothing Nothing
  containerAdd sw1 grid1

  return (sw1, widgets1)

macroscopicExamination :: IO (ScrolledWindow, [[Widget]], [Button])
macroscopicExamination = do
  let n2 = length labels2 -- TODO delete using 0..

  exps <- sequence $ replicate n2 (expanderNew "")

  buts <- sequence [do
                      b <- buttonNewWithLabel l
                      expanderSetLabelWidget e b
                      return b | (e, l) <- zip exps labels2]

  def2 <- defInner2
  (_, widgets2) <- unzip <$> sequence
            [ initGrid (length lab) lab (return def) >>= (\(g, e, w) -> containerAdd ex g >> return (e, w))
            | (ex, lab, def) <- zip3 exps labelsInner2 def2 ]
  fillings2 widgets2

  grid2 <- gridNew
  containerSetBorderWidth grid2 2
  gridSetRowSpacing grid2 2
  sequence_ [gridAttach grid2 e 0 i 2 1 | (e, i) <- zip exps [0..n2 - 1]]

  sw2 <- scrolledWindowNew Nothing Nothing

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

  void $ choice2 `on` buttonActivated $ do
    containerRemove sw2 box2
    containerAdd sw2 frame
    widgetShowAll sw2

  containerAdd sw2 box2

  return (sw2, widgets2, buts)

startGUI :: IO ()
startGUI = do
  void initGUI

  window <- windowNew
  set window [ windowTitle          := "ПаТаН" ]

  saveRTF <- buttonNewWithLabel "Сохранить в RTF"
  saveToEx <- buttonNewWithLabel "Сохранить в Excel"
  saveToGS <- buttonNewWithLabel "Сохранить в Google Spreadsheet"

  set saveRTF [ widgetHExpand := True ]
  set saveToEx [ widgetHExpand := True ]
  set saveToGS [ widgetHExpand := True ]

  -- TODO add "Save to GS"

  butBox <- hBoxNew True 0
  containerAdd butBox saveRTF
  containerAdd butBox saveToEx
  containerAdd butBox saveToGS

  note <- notebookNew

  (sw1, widgets1) <- clinicalData
  (sw2, widgets2, buts) <- macroscopicExamination

  void $ notebookAppendPage note sw1 "Клинические данные"
  void $ notebookAppendPage note sw2 "Макроскопическое исследование"

  menuBar <- createMenuBar menuBarDescr

  vbox <- vBoxNew False 0

  grid <- gridNew
  -- set grid [ containerBorderWidth := 4 ]

  gridAttach grid butBox 0 0 1 1
  gridAttach grid note   0 1 1 1

  containerAdd vbox menuBar
  containerAdd vbox grid

  containerAdd window vbox
  signalsMain saveRTF saveToEx saveToGS widgets1 widgets2

  windowMaximize window
  widgetShowAll window

  -- TODO need to speed up
  void $ window `on` configureEvent $ do
    (width, h) <- eventSize
    liftIO $ scrolledWindowSetMinContentHeight sw1 (h - 100)
    sequence_ [liftIO $ widgetSetSizeRequest b (width - 20) 10 | b <- buts]
    return False

  void $ window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна

  mainGUI -- start the main loop
