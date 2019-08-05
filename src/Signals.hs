module Signals where

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
  entrySetText (entries !! sexLabNum) sex
  entrySetText (entries !! 5) dep
  entrySetText (entries !! dateDeathLabNum) dateDeath
  return False

sign1sect :: Grid -> [Entry] -> IO ()
sign1sect grid entries = do
  fillings1 entries
  (Just tmp) <- gridGetChildAt grid 1 2
  let medRecCB = castToComboBox tmp
  medRecEC <- entryGetCompletion $ entries !! medRecLabNum

  _ <- medRecCB `on` changed $ do
    ti <- comboBoxGetActiveIter medRecCB
    case ti of
      Just i -> foo entries i >> return ()
      _      -> return ()
  _ <- medRecEC `on` matchSelected $ (\_ ti -> foo entries ti)

  comboBoxSetActive medRecCB 0

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
    -- mapM entryGetText entries2>>= writeText2 out
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()
  return ()
