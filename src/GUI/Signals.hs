module GUI.Signals where

import           Control.Monad          (void)
import           Control.Monad.Extra    (findM, fromMaybeM, maybeM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor           ((<&>))
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, pack)
import           Graphics.UI.Gtk        hiding (response)
import           System.Info
import           System.Process

import           Excel.GogolSheet
import           Excel.Xman
import           GUI.CommonGUI
import           GUI.Fillings
import           Utils.Labels
import           Utils.Writer

completeEntriesFromExcel :: [Entry] -> TreeIter -> IO Bool
completeEntriesFromExcel entries (TreeIter _ i _ _) = do
  [fio, sex, dep, age, dateDeath, datePsy] <- getAll (fromIntegral (i + 2))
  entrySetText (entries !! fioLabNum) fio
  Just grid   <- fmap castToGrid <$> maybeM undefined widgetGetParent (widgetGetParent (head entries))
  Just sexBox <- fmap castToContainer <$> gridGetChildAt grid 1 sexLabNum
  buttonClicked =<< castToButton <$> fromMaybeM undefined
                    (findM (\x -> (== sex) <$> (buttonGetLabel . castToButton) x)
                    =<< containerGetChildren sexBox)
  unfocLabel . fromJust =<< gridGetChildAt grid 0 sexLabNum
  entrySetText (entries !! depLabNum) dep
  entrySetText (entries !! ageLabNum) age
  entrySetText (entries !! dateDeathLabNum) dateDeath
  entrySetText (entries !! datePsyLabNum) datePsy
  return False

radioSignal :: RadioButton -> Entry -> Widget -> IO ()
radioSignal b e l = do
  void $ b `on` toggled $ focLabel l >> (entrySetText e =<< (buttonGetLabel b :: IO String))
  void $ b `on` focusOutEvent $ liftIO $ unfocLabel l >> return False
  return ()

addRadioButtonGroup :: Int -> String -> String -> Grid -> [Entry] -> IO ()
addRadioButtonGroup i fstS sndS grid entries = do
  box <- hBoxNew True 0
  h <- radioButtonNew
  f <- radioButtonNewWithLabelFromWidget h fstS
  Just lab <- gridGetChildAt grid 0 i
  s <- radioButtonNewWithLabelFromWidget f sndS
  let e = entries !! i
  radioSignal f e lab
  radioSignal s e lab
  boxPackStart box f PackGrow 20
  boxPackStart box s PackGrow 20

  Just was <- gridGetChildAt grid 1 i
  containerRemove grid was
  widgetSetName box "MyBox"
  gridAttach grid box 1 i 1 1
  widgetShowAll grid

signalsClinicalData :: Grid -> [Entry] -> IO ()
signalsClinicalData grid entries = do
  addRadioButtonGroup sexLabNum    "М"  "Ж"   grid entries
  addRadioButtonGroup wasDocLabNum "Да" "Нет" grid entries
  fillings1 entries

  Just medRecCB <- fmap castToComboBox <$> gridGetChildAt grid 1 medRecLabNum
  medRecEC <- entryGetCompletion $ entries !! medRecLabNum

  void $ medRecCB `on` changed $ do
    ti <- comboBoxGetActiveIter medRecCB
    case ti of
      Just i  -> completeEntriesFromExcel entries i >> return ()
      Nothing -> return ()
  void $ medRecEC `on` matchSelected $ (\_ ti -> completeEntriesFromExcel entries ti)

  comboBoxSetActive medRecCB 0

  st <- listStoreNew =<< diagnosX
  customStoreSetColumn st textColumn id
  ec <- entryCompletionNew
  set ec [ entryCompletionModel := Just st, entryCompletionMinimumKeyLength := 4, entryCompletionTextColumn := textColumn ]
  entrySetCompletion (last entries) ec


getText :: Widget -> IO String
getText rcol = do
  name <- widgetGetName rcol
  case name of
    "MyBox"    -> do
                  mbut <- findM (\x -> toggleButtonGetActive x)
                          =<< return . map castToToggleButton
                          =<< containerGetChildren (castToContainer rcol)
                  case mbut of
                    Just but -> buttonGetLabel but
                    Nothing  -> return ""

    "GtkFrame" -> do
                  tb <- textViewGetBuffer =<< getTV rcol
                  st <- textBufferGetStartIter tb
                  end <- textBufferGetEndIter tb
                  textIterGetText st end
    _          -> getEntry rcol >>= entryGetText

signalsMain :: Button -> Button -> Button -> [Widget] -> [[Widget]] -> IO ()
signalsMain saveRTF saveToEx saveToGS widgets1 widgets2 = do
  window <- fmap castToWindow <$>
            (widgetGetParent . fromJust
            =<< widgetGetParent . fromJust
            =<< widgetGetParent . fromJust
            =<< widgetGetParent saveRTF)

  void $ saveRTF `on` buttonActivated $ do
    dialog <- fileChooserDialogNew
              (Just "Choose where save your Protocol")
              window
              FileChooserActionSave
              [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)]
    -- TODO add doctor surname to defName
    defName <- getText (head widgets1) <&> (++ ".rtf")
    fileChooserSetCurrentName dialog defName
    widgetShowAll dialog

    response <- dialogRun dialog
    case response of
      ResponseAccept      -> do Just pathFile <- fileChooserGetFilename dialog
                                putStrLn pathFile
                                out <- initRTF pathFile
                                writeHeaderTable out
                                writeText1 out =<< mapM getText widgets1
                                writeText2 out =<< mapM (mapM getText) widgets2
                                endRTF out
                                where
                                  open pathFile
                                    = case os of
                                        "linux"   -> runCommand ("xdg-open " ++ pathFile)
                                        "mingw32" -> runCommand ("start "    ++ pathFile)
                                        _         -> undefined
      _                   -> return ()
    widgetHide dialog

  void $ saveToEx `on` buttonActivated $ do
    dialog <- fileChooserDialogNew
              (Just "Choose Excel")
              window
              FileChooserActionOpen
              [("gtk-cancel", ResponseCancel), ("gtk-ok", ResponseOk)]
    widgetShow dialog

    response <- dialogRun dialog
    case response of
      ResponseOk          -> do Just pathFile <- fileChooserGetFilename dialog
                                writeToEx pathFile
      _                   -> return ()
    widgetHide dialog

  void $ saveToGS `on` buttonActivated $ do
    dialog <- dialogNew
    let dialogTitle        = "Введите ссылку на Google Spreadsheet"
        titleWidthInpixels = length dialogTitle * 10
    set dialog [ windowTitle        := dialogTitle
               , windowResizable    := False
               , windowDefaultWidth := titleWidthInpixels ]
    linkEntry <- entryNew
    entrySetText linkEntry "https://docs.google.com/spreadsheets/d/1d5s6kHQCsdU3xPDMesmEDS_H3XrcMd31vRNqphI3ebQ/edit#gid=1660848898"
    void $ linkEntry `on` entryActivated $ dialogResponse dialog ResponseOk

    containerRemove dialog =<< head <$> containerGetChildren dialog -- remove def VBox
    containerAdd dialog linkEntry
    widgetShowAll dialog

    response <- dialogRun dialog
    case response of
      ResponseOk -> do link <- entryGetText linkEntry
                       writeToGS link . map pack =<< mapM getText widgets1
      _          -> return ()
    widgetHide dialog
