module Signals where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Text              (unpack)
import           Graphics.UI.Gtk
import           System.IO
import           System.Process

import           Fillings
import           Labels
import           Writer
import           Xman

foo :: [Entry] -> TreeIter -> IO Bool
foo entries (TreeIter _ i _ _) = do
  -- putStrLn $ show (i + 2)
  [fio, sex, dep, dateDeath] <- getAll (fromIntegral (i + 2))
  entrySetText (entries !! fioLabNum) fio
  entrySetText (entries !! sexLabNum) sex -- TODO
  entrySetText (entries !! 5) dep
  entrySetText (entries !! dateDeathLabNum) dateDeath
  return False

bar :: Int -> String -> String -> Grid -> [Entry] -> IO ()
bar i fst snd grid entries = do
  box <- hBoxNew True 2
  h <- radioButtonNew
  f <- radioButtonNewWithLabel fst
  radioButtonSetGroup f h
  f `on` toggled $ entrySetText (entries !! i) fst
  s <- radioButtonNewWithLabelFromWidget f snd
  s `on` toggled $ entrySetText (entries !! i) snd
  boxPackStart box f PackGrow 2
  boxPackStart box s PackGrow 2

  (Just was) <- gridGetChildAt grid 1 i
  containerRemove grid was
  gridAttach grid box 1 i 1 1
  widgetShowAll grid

colorOnFocus :: Int -> IO (Maybe Widget) -> Entry -> IO ()
colorOnFocus i mw en = do
  Just w <- mw
  bgWas <- newIORef undefined
  en `on` focusInEvent $ liftIO $ do
    widgetGetStyle w >>= (flip styleGetBackground) StateNormal >>= writeIORef bgWas
    widgetModifyBg w StateNormal (Color 30000 123 125)
    return False
  en `on` focusOutEvent $ do
    liftIO $ readIORef bgWas >>= widgetModifyBg w StateNormal
    return False
  return ()

sign1sect :: Int -> Grid -> [Entry] -> IO ()
sign1sect n1 grid entries = do
  bar sexLabNum "Мужской" "Женский" grid entries
  bar 17 "Да" "Нет" grid entries

  fillings1 entries
  (Just tmp) <- gridGetChildAt grid 1 2
  let medRecCB = castToComboBox tmp
  medRecEC <- entryGetCompletion $ entries !! medRecLabNum

  _ <- medRecCB `on` changed $ do
    ti <- comboBoxGetActiveIter medRecCB
    case ti of
      Just i  -> foo entries i >> return ()
      Nothing -> return ()
  _ <- medRecEC `on` matchSelected $ (\_ ti -> foo entries ti)

  comboBoxSetActive medRecCB 0

  sequence_ [colorOnFocus i (gridGetChildAt grid 0 i) en | (en, i) <- zip entries [0..n1 - 1]]

signSectChange :: ScrolledWindow -> [Grid] -> [Button] -> [[Entry]] -> IO ()
signSectChange sw [grid1, grid2] [butt1_2, butt2_1, butt2_3] [entries1] = do
  _ <- butt1_2 `on` buttonActivated $ do
    containerRemove sw grid1
    containerAdd sw grid2
    widgetShowAll sw

  _ <- butt2_1 `on` buttonActivated $ do
    containerRemove sw grid2
    containerAdd sw grid1
    widgetShowAll sw

  _ <- butt2_3 `on` buttonActivated $ do
    out <- initRTF
    mapM entryGetText entries1 >>= writeText1 out
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()
  return ()
