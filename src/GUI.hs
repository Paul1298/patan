{-# LANGUAGE ScopedTypeVariables #-}
module GUI where

import           CommonGUI
import           Control.Monad.IO.Class (liftIO)
import           DefCombo
import           Graphics.UI.Gtk
import           Labels
import           Signals

startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle          := "ПаТаН"
             -- , windowDefaultWidth   := 820
             -- , windowDefaultHeight  := 1080
             , containerBorderWidth := 4
             ]
  let n1 = length labels1
  (grid1, entries1) <- initGrid n1 labels1 initDef1
  sign1sect grid1 entries1

  -- gridSetColumnHomogeneous grid2 True
  let n2 = length labels2

  exps <- sequence $ replicate n2 (expanderNew "")

  buts <- sequence [do
                      b <- buttonNewWithLabel l
                      -- set b [ widgetHExpand := True ]
                      -- set e [ widgetHExpand := True ]
                      expanderSetLabelWidget e b
                      return b | (e, l) <- zip exps labels2]

  ready <- buttonNewWithLabel "Сохранить документ"
  set ready [ widgetHExpand := True ]
  signSectChange ready entries1

  _ <- window `on` configureEvent $ do
    (width, _) <- eventSize
    sequence_ [liftIO $ widgetSetSizeRequest b (width - 40) 10 | b <- buts]
    return False

  def2 <- defInner2
  sequence_ [initGrid (length lab) lab (return def) >>= (\(g, _) -> containerAdd ex g)
            | (ex, lab, def) <- zip3 exps labelsInner2 def2 ]

  grid2 <- gridNew
  containerSetBorderWidth grid2 2
  gridSetRowSpacing grid2 2
  sequence_ [gridAttach grid2 e 0 i 2 1 | (e, i) <- zip exps [0..n2 - 1]] --attach them


  note <- notebookNew
  -- sw1 <- scrolledWindowNew Nothing Nothing
  -- containerAdd sw1 grid1
  notebookAppendPage note grid1 "Клинические данные"

  sw2 <- scrolledWindowNew Nothing Nothing
  containerAdd sw2 grid2
  notebookAppendPage note sw2 "Макроскопическое исследование"


  grid <- gridNew
  gridAttach grid ready 0 0 1 1
  gridAttach grid note 0 1 1 1

  buf <- textBufferNew Nothing
  tv <- textViewNewWithBuffer buf

  tv `on` insertAtCursor $ \(str :: String) -> do
    textBufferGetEndIter buf >>= textIterGetOffset >>= putStrLn . show
  -- textBufferGetIterAtLine
  -- textIterGetLineOffset
  -- entrySetMaxLength en 20
  -- gridAttach grid tv 0 0 1 1
  containerAdd window grid

  widgetShowAll window

  windowMaximize window
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна
  -- _ <- window `on` focusOutEvent $ do -- TODO add close calendar
  --   liftIO $ putStrLn "sdf"
  --   return False
  return ()
