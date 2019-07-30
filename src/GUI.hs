{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI where
-- import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Text              (Text, append, pack)
-- import           Data.Text.Lazy         (toStrict)
-- import           Formatting             (format, hex, stext, (%))
import           Graphics.UI.Gtk
import           System.IO
import           System.Process
import           Text.Printf            (printf)

import           DefCombo
import           Labels
import           Writer                 hiding (format)
import           Xman

getEntry :: ComboBox -> IO Entry
getEntry c = do
  Just w <- binGetChild c
  return (castToEntry w)

foo :: [Entry] -> TreeIter -> IO Bool
foo entries (TreeIter _ i _ _) = do
  -- putStrLn $ show (i + 2)
  [fio, sex, dep, dateDeath] <- getAll (fromIntegral (i + 2))
  entrySetText (entries !! fioLabNum) fio
  entrySetText (entries !! sexLabNum) sex
  entrySetText (entries !! 5) dep
  entrySetText (entries !! dateDeathLabNum) dateDeath
  return False

fillDates :: [Entry] -> IO ()
fillDates entries = do
  mapM_ fill entries
  where
    fill :: Entry -> IO ()
    fill entry = do
      entrySetText entry ("__.__.____" :: Text) -- default

      idRef <- newIORef undefined
      id <- entry `on` insertText $ \(str :: Text) pos -> do
        id <- readIORef idRef
        signalBlock id
        pos' <- newIORef 0
        if (pos == 2) || (pos == 5)
          then do
            editableDeleteText entry (pos + 1) (pos + 2)
            editableInsertText entry str (pos + 1) >>= writeIORef pos'
            -- return ()
          else do
            editableDeleteText entry pos (pos + 1)
            editableInsertText entry str pos >>= writeIORef pos'
            -- return ()
        signalUnblock id
        stopInsertText id
        readIORef pos'
      writeIORef idRef id


-- signals section
signals :: [Entry] -> [EntryCompletion] -> [ComboBox] -> CheckButton -> Window -> IO (ConnectId Window)
signals entries ecompls combos checkb window = do
  fillDates [ entries !! dateRepLabNum
            , entries !! dateBirthLabNum
            , entries !! dateDeathLabNum
            , entries !! dateRecLabNum
            ]
  let
    medRecEC = ecompls !! medRecLabNum
    medRecCB = combos !! medRecLabNum
    -- fioEC = ecompls !! 3

  _ <- medRecEC `on` matchSelected $ (\_ ti -> foo entries ti)
  _ <- medRecCB `on` changed $ do
    ti <- comboBoxGetActiveIter medRecCB
    case ti of
      Just i -> foo entries i >> return ()
      _      -> return ()

  -- Готово
  _ <- checkb `on` toggled $ do
    out <- initRTF
    mapM entryGetText entries >>= writeText1 out
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()

  -- Закрытие окна
  window `on` deleteEvent $ liftIO mainQuit >> return False


startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle          := ("ПаТаН" :: Text) ]
  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True

  let n = length labels1

  -- labels
  labels <- sequence [labelNew $ Just l | l <- labels1] -- init labels
  sequence_ [miscSetAlignment l 0 0 | l <- labels] -- left alignment labels
  sequence_ [gridAttach grid1 l 0 i 2 1 | (l, i) <- zip labels [0..n - 1]] --attach them

  let textColumn = makeColumnIdString 0

  -- entries with comboBox
  def1 <- initDef1
  stores <- sequence [listStoreNew def | def <- def1]
  sequence_ [customStoreSetColumn store textColumn id | store <- stores ] -- set the extraction function

  -- combos
  combos <- sequence [comboBoxNewWithModelAndEntry store | store <- stores ]
  sequence_ [comboBoxSetEntryTextColumn combo textColumn | combo <- combos ] -- set which column should be used
  sequence_ [gridAttach grid1 field 2 i 3 1 | (field, i) <- zip combos [0..n - 1]]

  -- entries
  entries <- mapM getEntry combos

  -- entry-completion
  ecompls <- sequence $ replicate n entryCompletionNew
  sequence_ [set ec [ entryCompletionModel            := Just st
                    , entryCompletionMinimumKeyLength := 0
                    , entryCompletionTextColumn       := textColumn] | (ec, st) <- zip ecompls stores]

  -- sequence_ [entryCompletionSetMinimumKeyLength ec 0 | ec <- ecompls]
  sequence_ [entrySetCompletion e ec | (e, ec) <- zip entries ecompls]

  checkb <- checkButtonNewWithLabel ("Готово" :: Text)
  gridAttach grid1 checkb 4 (n + 1) 1 1

  -- cal <- calendarNew
  -- _ <- onDaySelectedDoubleClick cal $ do
  --   (y, m, d) <- calendarGetDate cal
  --   let date = (printf "%02d" d) ++ "." ++ (printf "%02d" m) ++ "." ++ (show y)
  --   entrySetText (entries !! 1) date
  --   return ()
  -- gridAttach grid1 cal 0 (n + 1) 1 1

  containerAdd window grid1
  _ <- signals entries ecompls combos checkb window -- TODO add cbs

  widgetShowAll window
  return ()
