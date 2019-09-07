module GUI.Signals where

import           Control.Monad          (void)
import           Control.Monad.Extra    (findM, fromMaybeM, maybeM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)
import           Graphics.UI.Gtk
import           System.Info
import           System.Process

import           Excel.Xman
import           GUI.CommonGUI
import           GUI.Fillings
import           Utils.Labels
import           Utils.Writer

foo :: [Entry] -> TreeIter -> IO Bool
foo entries (TreeIter _ i _ _) = do
  -- putStrLn $ show (i + 2)
  [fio, sex, dep, age, dateDeath, datePsy] <- getAll (fromIntegral (i + 2))
  entrySetText (entries !! fioLabNum) fio
  Just grid   <- fmap castToGrid <$> maybeM undefined widgetGetParent (widgetGetParent (head entries))
  Just sexBox <- fmap castToContainer <$> gridGetChildAt grid 1 sexLabNum
  buttonClicked =<< castToButton <$> fromMaybeM undefined
                    (findM (\x -> (== sex) <$> (buttonGetLabel . castToButton) x)
                    =<< containerGetChildren sexBox)
  unfocLabel . fromJust =<< gridGetChildAt grid 0 sexLabNum
  entrySetText (entries !! 5) dep
  entrySetText (entries !! ageLabNum) age
  entrySetText (entries !! dateDeathLabNum) dateDeath
  entrySetText (entries !! datePsyLabNum) datePsy
  return False

radioSign :: RadioButton -> Entry -> Widget -> IO ()
radioSign b e l = do
  void $ b `on` toggled $ focLabel l >> (entrySetText e =<< (buttonGetLabel b :: IO String))
  void $ b `on` focusOutEvent $ liftIO $ unfocLabel l >> return False
  return ()

bar :: Int -> String -> String -> Grid -> [Entry] -> IO ()
bar i fstS sndS grid entries = do
  box <- hBoxNew True 0
  h <- radioButtonNew
  f <- radioButtonNewWithLabelFromWidget h fstS
  Just lab <- gridGetChildAt grid 0 i
  s <- radioButtonNewWithLabelFromWidget f sndS
  let e = entries !! i
  radioSign f e lab
  radioSign s e lab
  boxPackStart box f PackGrow 20
  boxPackStart box s PackGrow 20

  Just was <- gridGetChildAt grid 1 i
  containerRemove grid was
  widgetSetName box "MyBox"
  gridAttach grid box 1 i 1 1
  widgetShowAll grid

sign1sect :: Grid -> [Entry] -> IO ()
sign1sect grid entries = do
  bar sexLabNum "М" "Ж" grid entries
  bar 19 "Да" "Нет" grid entries
  fillings1 entries

  Just medRecCB <- fmap castToComboBox <$> gridGetChildAt grid 1 medRecLabNum
  medRecEC <- entryGetCompletion $ entries !! medRecLabNum

  void $ medRecCB `on` changed $ do
    ti <- comboBoxGetActiveIter medRecCB
    case ti of
      Just i  -> foo entries i >> return ()
      Nothing -> return ()
  void $ medRecEC `on` matchSelected $ (\_ ti -> foo entries ti)

  comboBoxSetActive medRecCB 0
  -- (entryGetText (entries !! sexLabNum) :: IO String) >>= putStrLn

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

signSectChange :: Button -> [Widget] -> [[Widget]] -> IO ()
signSectChange ready widgets1 widgets2 = do
  void $ ready `on` buttonActivated $ do
    window <- fmap castToWindow <$> maybeM undefined widgetGetParent (widgetGetParent ready)
    dialog <- fileChooserDialogNew (Just "Choose where save your Protocol") window
              FileChooserActionSave [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)]
    fileChooserSetCurrentName dialog "test.rtf"
    widgetShowAll dialog
    response <- dialogRun dialog
    -- fileChooserSetCurrentFolder dialog "../"
    -- fileChooserSetFilename dialog "test.rtf"
    case response of
      ResponseAccept      -> do Just pathFile <- fileChooserGetFilename dialog
                                out <- initRTF pathFile
                                writeHeaderTable out
                                writeText1 out =<< mapM getText widgets1
                                writeText2 out =<< mapM (mapM getText) widgets2
                                endRTF out
                                case os of
                                  "linux"   -> createProcess (proc "loffice" [pathFile]) >> return ()
                                  "mingw32" -> runCommand ("start " ++ pathFile) >> return ()
                                  _         -> undefined
                                return ()
      ResponseCancel      -> putStrLn "dialog canceled"
      ResponseDeleteEvent -> putStrLn "dialog closed"
    widgetHide dialog
