module Signals where

import           Control.Monad.Extra    (findM, fromMaybeM, maybeM)
import           Control.Monad.IO.Class (liftIO)
import           Data.List.Utils        (replace)
import           Data.Maybe             (fromJust)
import           Graphics.UI.Gtk
import           System.Info
import           System.Process

import           CommonGUI
import           Fillings
import           Labels
import           Writer
import           Xman

foo :: [Entry] -> TreeIter -> IO Bool
foo entries (TreeIter _ i _ _) = do
  -- putStrLn $ show (i + 2)
  [fio, sex, dep, age, dateDeath, datePsy] <- getAll (fromIntegral (i + 2))
  entrySetText (entries !! fioLabNum) fio
  -- entrySetText (entries !! sexLabNum) sex -- TODO
  -- (entries !! sexLabNum) `on` editableChanged $ do
  --   entryGetText (entries !! sexLabNum) >>= putStrLn
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
  _ <- b `on` toggled $ focLabel l >> (entrySetText e =<< (buttonGetLabel b :: IO String))
  _ <- b `on` focusOutEvent $ liftIO $ unfocLabel l >> return False
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

  _ <- medRecCB `on` changed $ do
    ti <- comboBoxGetActiveIter medRecCB
    case ti of
      Just i  -> foo entries i >> return ()
      Nothing -> return ()
  _ <- medRecEC `on` matchSelected $ (\_ ti -> foo entries ti)

  comboBoxSetActive medRecCB 0
  -- (entryGetText (entries !! sexLabNum) :: IO String) >>= putStrLn

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
                  tb <- textViewGetBuffer . castToTextView . head
                        =<< containerGetChildren (castToContainer rcol)
                  st <- textBufferGetStartIter tb
                  end <- textBufferGetEndIter tb
                  replace "\n" "\\line " <$> textIterGetText st end
    _          -> getEntry rcol >>= entryGetText

signSectChange :: Button -> [Widget] -> [[Entry]] -> IO ()
signSectChange ready widgets1 entries2 = do
  _ <- ready `on` buttonActivated $ do
    out <- initRTF
    writeHeaderTable out
    mapM getText widgets1 >>= writeText1 out
    mapM (mapM entryGetText) entries2 >>= writeText2 out
    endRTF out
    case os of
      "linux"   -> createProcess (proc "loffice" [pathFile]) >> return ()
      "windows" -> runCommand ("start " ++ pathFile) >> return ()
      _         -> undefined
  return ()
