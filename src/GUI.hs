{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI where
import           Control.Monad          ()
import           Control.Monad.IO.Class
import           Data.Char              (isDigit)
import           Data.IORef
import qualified Data.List              as L (length)
import           Data.Text              (Text, drop, length, take, unpack)
import           Graphics.UI.Gtk
import           Prelude                hiding (drop, length, take)
import           System.IO
import           System.Process
import           Text.Printf            ()

import           DefCombo
import           Labels
import           Writer                 hiding (format)
import           Xman


fillDates :: [Entry] -> IO ()
fillDates entries = do
  mapM_ fill entries
  where
    fill :: Entry -> IO ()
    fill entry = do
      let def = "__.__.____" :: Text

      entrySetText entry def

      editWrite <- newIORef True
      editRead <- newIORef True

      idIRef <- newIORef undefined
      idI <- entry `on` insertText $ \(str :: Text) pos -> do
        -- putStrLn $ readIORef flag
        idI <- readIORef idIRef
        signalBlock idI
        -- (entryGetText entry :: IO String) >>= length
        pos' <- newIORef pos
        flag <- readIORef editWrite
        -- putStrLn $ show f
        if flag
        then
          if (all (\x -> isDigit x || x == '.') $ unpack str)
          then
            if (pos >= 10)
            then writeIORef pos' 10
            else
              if (pos == 2) || (pos == 5)
              then do
                p <- editableInsertText entry str (pos + 1)
                writeIORef pos' p
                writeIORef editRead False
                editableDeleteText entry p (p + length str)
                writeIORef editRead True
              else do
                p <- editableInsertText entry str pos
                -- putStrLn ((unpack str) ++ " on " ++ show pos)
                writeIORef pos' p
                writeIORef editRead False
                editableDeleteText entry p (p + length str)
                writeIORef editRead True
          else return ()
        else editableInsertText entry str pos >>= writeIORef pos'

        signalUnblock idI
        stopInsertText idI
        readIORef pos'
      writeIORef idIRef idI

      idDRef <- newIORef undefined
      idD <- entry `on` deleteText $ \startPos endPos -> do
        -- len <- liftM length (entryGetText entry :: IO String)
        idD <- readIORef idDRef
        signalBlock idD
        -- putStrLn (show startPos ++ " -- " ++ show endPos)
        editableDeleteText entry startPos endPos
        let tmp = if (endPos < 0) then 10 else endPos
        -- entryGetText entry >>= putStrLn
        flag <- readIORef editRead
        if flag
        then
          if (startPos > 9)
          then return ()
          else do
            let substr = take (tmp - startPos) (drop startPos def)
            -- putStrLn $ unpack substr
            writeIORef editWrite False
            _ <- editableInsertText entry substr startPos
            writeIORef editWrite True
        else return ()
        signalUnblock idD
        stopDeleteText idD
      writeIORef idDRef idD

fillings :: [Entry] -> IO ()
fillings entries = do
  fillDates [ entries !! dateRepLabNum
            , entries !! dateBirthLabNum
            , entries !! dateDeathLabNum
            , entries !! dateRecLabNum
            , entries !! datePsyLabNum
            ]
  entrySetText (entries !! 0) ("1" :: Text)
  entrySetText (entries !! 4) ("Областное бюджетное учреждение здравоохранения «Курская городская клиническая больница скорой медицинской помощи»" :: Text)
  entrySetText (entries !! 20) ("Указаны в посмертном клиническом эпикризе в истории болезни." :: Text)

foo :: [Entry] -> TreeIter -> IO Bool
foo entries (TreeIter _ i _ _) = do
  -- putStrLn $ show (i + 2)
  [fio, sex, dep, dateDeath] <- getAll (fromIntegral (i + 2))
  entrySetText (entries !! fioLabNum) fio
  entrySetText (entries !! sexLabNum) sex
  entrySetText (entries !! 5) dep
  entrySetText (entries !! dateDeathLabNum) dateDeath
  return False

-- signals section
signals :: [Entry] -> [EntryCompletion] -> [ComboBox] -> Button -> Window -> IO ()
signals entries ecompls combos ready window = do
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

  comboBoxSetActive (combos !! 2) 0

  -- Готово
  _ <- ready `on` buttonActivated $ do
    out <- initRTF
    mapM entryGetText entries >>= writeText1 out
    endRTF out
    hClose out
    _ <- createProcess (proc "loffice" [pathFile]) --linux
    -- _ <- runCommand ("start " ++ pathFile) --win
    return ()

  -- Закрытие окна
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
  return ()

getEntry :: ComboBox -> IO Entry
getEntry c = do
  Just w <- binGetChild c
  return (castToEntry w)

startGUI :: IO ()
startGUI = do
  window <- windowNew
  set window [ windowTitle := ("ПаТаН" :: Text) ]
  grid1 <- gridNew
  -- gridSetRowHomogeneous grid1 True -- rows same height
  gridSetColumnHomogeneous grid1 True

  let n = L.length labels1

  -- labels
  labels <- sequence [labelNew $ Just l | l <- labels1] -- init labels
  sequence_ [miscSetAlignment l 0 0 | l <- labels] -- left alignment labels
  -- sequence_ [labelSetSingleLineMode l False | l <- labels] -- sets the desired width in character
  sequence_ [gridAttach grid1 l 0 i 1 1 | (l, i) <- zip labels [0..n - 1]] --attach them

  let textColumn = makeColumnIdString 0

  -- entries with comboBox
  def1 <- initDef1
  stores <- sequence [listStoreNew def | def <- def1]
  sequence_ [customStoreSetColumn store textColumn id | store <- stores ] -- set the extraction function

  -- combos
  combos <- sequence [comboBoxNewWithModelAndEntry store | store <- stores ]
  sequence_ [comboBoxSetEntryTextColumn combo textColumn | combo <- combos ] -- set which column should be used
  sequence_ [gridAttach grid1 field 1 i 2 1 | (field, i) <- zip combos [0..n - 1]]

  -- readsaddy <- buttonNewWithLabel ("Готово" :: Text)
  -- gridAttach grid1 (readsaddy) 1 0 1 1

  -- entries
  entries <- mapM getEntry combos

  -- entry-completion
  ecompls <- sequence $ replicate n entryCompletionNew
  sequence_ [set ec [ entryCompletionModel            := Just st
                    , entryCompletionMinimumKeyLength := 0
                    , entryCompletionTextColumn       := textColumn] | (ec, st) <- zip ecompls stores]

  -- sequence_ [entryCompletionSetMinimumKeyLength ec 0 | ec <- ecompls]
  sequence_ [entrySetCompletion e ec | (e, ec) <- zip entries ecompls]

  ready <- buttonNewWithLabel ("Готово" :: Text)
  gridAttach grid1 ready 2 (n + 1) 1 1

  -- nextBut <- buttonNewWithLabel ("Патологоанатомическое вскрытие" :: Text)
  -- gridAttach grid1 nextBut 2 (n + 1) 1 1

  -- cal <- calendarNew
  -- _ <- onDaySelectedDoubleClick cal $ do
  --   (y, m, d) <- calendarGetDate cal
  --   let date = (printf "%02d" d) ++ "." ++ (printf "%02d" m) ++ "." ++ (printf "%04d" y)
  --   entrySetText (entries !! dateRepLabNum) date
  --   return ()
  -- gridAttach grid1 cal 0 (n + 1) 1 1

  containerAdd window grid1
  fillings entries


  signals entries ecompls combos ready window -- TODO add cbs
  widgetSetCanFocus (labels !! 0) True
  widgetGrabFocus (labels !! 0)
  widgetShowAll window
  -- windowMaximize window
  return ()
