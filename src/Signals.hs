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
  [fio, sex, dep, age, dateDeath, datePsy] <- getAll (fromIntegral (i + 2))
  entrySetText (entries !! fioLabNum) fio
  entrySetText (entries !! sexLabNum) sex -- TODO
  -- (entries !! sexLabNum) `on` editableChanged $ do
  --   entryGetText (entries !! sexLabNum) >>= putStrLn
  entrySetText (entries !! 5) dep
  entrySetText (entries !! ageLabNum) age
  entrySetText (entries !! dateDeathLabNum) dateDeath
  entrySetText (entries !! datePsyLabNum) datePsy
  return False

bar :: Int -> String -> String -> Grid -> [Entry] -> IO ()
bar i fstS sndS grid entries = do
  box <- hBoxNew True 2
  h <- radioButtonNew
  f <- radioButtonNewWithLabelFromWidget h fstS
  _ <- f `on` toggled $ entrySetText (entries !! i) fstS
  s <- radioButtonNewWithLabelFromWidget f sndS
  _ <- s `on` toggled $ entrySetText (entries !! i) sndS
  boxPackStart box f PackGrow 2
  boxPackStart box s PackGrow 2

  (Just was) <- gridGetChildAt grid 1 i
  containerRemove grid was
  gridAttach grid box 1 i 1 1
  widgetShowAll grid

sign1sect :: Grid -> [Entry] -> IO ()
sign1sect grid entries = do
  bar sexLabNum "Мужской" "Женский" grid entries
  bar 19 "Да" "Нет" grid entries
  fillings1 entries

  -- sp <- spinButtonNewWithRange 0 90 1
  -- Just tmp <- widgetGetParent $ entries !! ageLabNum
  -- containerRemove grid tmp
  -- gridAttach grid sp 1 ageLabNum 1 1

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
signSectChange _ _ _ _ = return ()
