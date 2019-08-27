{-# LANGUAGE ScopedTypeVariables #-}
module Fillings where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isDigit)
import           Data.Foldable          (foldlM)
import           Data.IORef
import           Graphics.UI.Gtk
import           Text.Printf

import           Labels

onlyInteger :: [Entry] -> IO ()
onlyInteger entries = do
  mapM_ only entries
  where
    only :: Entry -> IO ()
    only entry = do
      idIRef <- newIORef undefined
      idI <- entry `on` insertText $ \(str :: String) pos -> do
        idI <- readIORef idIRef
        signalBlock idI
        pos' <- newIORef pos
        if (all isDigit str)
        then editableInsertText entry str pos >>= writeIORef pos'
        else return ()

        signalUnblock idI
        stopInsertText idI
        readIORef pos'
      writeIORef idIRef idI

pattern :: Entry -> String -> [Int] -> IO ()
pattern entry def invis = do
  entrySetText entry def
  let n = length def

  editWrite <- newIORef True
  editRead <- newIORef True

  idIRef <- newIORef undefined
  idI <- entry `on` insertText $ \(str :: String) pos -> do
    let ns = length str
    idI <- readIORef idIRef
    if (ns > 1)
    then do
      stopInsertText idI
      foldlM (\i c -> editableInsertText entry [c] i) pos str
    else do
      signalBlock idI
      pos' <- newIORef pos
      flag <- readIORef editWrite
      if flag
      then if (all isDigit str)
        then if (pos >= n)
          then writeIORef pos' pos
          else do
            let npos = nextPos pos $ dropWhile (/= pos) invis
            if (npos >= n) then return ()
            else do
              p <- editableInsertText entry str npos
              writeIORef pos' p
              writeIORef editRead False
              editableDeleteText entry p (p + ns)
              writeIORef editRead True
        else return ()
      else editableInsertText entry str pos >>= writeIORef pos'

      signalUnblock idI
      stopInsertText idI
      readIORef pos'
  writeIORef idIRef idI

  idDRef <- newIORef undefined
  idD <- entry `on` deleteText $ \startPos endPos -> do
    idD <- readIORef idDRef
    signalBlock idD
    editableDeleteText entry startPos endPos
    let tmp = if (endPos < 0) then n else endPos
    flag <- readIORef editRead
    if flag
    then
      if (startPos >= n)
      then return ()
      else do
        let substr = take (tmp - startPos) (drop startPos def)
        writeIORef editWrite False
        void $ editableInsertText entry substr startPos
        writeIORef editWrite True
    else return ()
    signalUnblock idD
    stopDeleteText idD
  writeIORef idDRef idD
  where
    nextPos :: Int -> [Int] -> Int
    nextPos pos []     = pos
    nextPos _ [i]      = i + 1 -- TODO fix this in the end of str
    nextPos _ (i : is) = if (head is == succ i) then nextPos undefined is else i + 1

addCalendar :: Entry -> IO ()
addCalendar entry = do
  cal <- calendarNew

  void $ onDaySelected cal $ do
    (y, m, d) <- calendarGetDate cal
    let date = printf "%02d%02d%04d" d (m + 1) y :: String
    entrySetText entry date

  (_, _, today) <- calendarGetDate cal
  calendarSelectDay cal today

  Just box <- fmap castToHBox <$> widgetGetParent entry
  vbox <- vBoxNew False 1
  containerSetResizeMode vbox ResizeQueue
  void $ vbox `on` showSignal $ widgetHide cal

  but   <- buttonNew
  image <- imageNewFromFile "download.jpeg"
  containerAdd but image
  widgetSetSizeRequest but 160 10

  void $ but `on` buttonActivated $ do
    widgetGrabFocus cal
    -- TODO add color label
    wis <- get cal widgetVisible
    if wis
    then widgetHide cal
    else widgetShow cal
  boxPackStart vbox but PackGrow 0
  boxPackStart vbox cal PackNatural 0
  boxPackEnd box vbox PackNatural 0

  void $ cal `on` focusOutEvent $ liftIO $ widgetHide cal >> return False
  return ()

fillDates :: [Entry] -> IO ()
fillDates entries = do
  mapM_ (\x -> pattern x "__.__.____ г." [2, 5, 10, 11, 12]) entries
  mapM_ addCalendar entries

fillDatesWithTime :: [Entry] -> IO ()
fillDatesWithTime entries = do
  mapM_ (\x -> pattern x "__.__.____ г. __-__" [2, 5, 10, 11, 12, 13, 16]) entries
  mapM_ addCalendar entries

fillings1 :: [Entry] -> IO ()
fillings1 entries = do
  fillDates [ entries !! dateRepLabNum
            , entries !! dateBirthLabNum
            , entries !! dateRecLabNum
            , entries !! datePsyLabNum
            ]
  fillDatesWithTime [
                      entries !! dateDeathLabNum
                    ]
  onlyInteger [ head entries
              , entries !! medRecLabNum
              , entries !! ageLabNum
              , entries !! 10
              ]
  entrySetText (entries !! 0) "1"
  entrySetText (entries !! 4) "Областное бюджетное учреждение здравоохранения «Курская городская клиническая больница скорой медицинской помощи»"
  entrySetText (entries !! 21) "Указаны в посмертном клиническом эпикризе в истории болезни."

fillBody :: [Entry] -> IO ()
fillBody ens =
  sequence_ $ zipWith (\e s -> pattern e s [5..100]) ens ["    пола", "    см.", "    кг."]

fillings2 :: [[Entry]] -> IO ()
fillings2 (body : es) = do
  fillBody body

fillTV :: Widget -> [String] -> IO ()
fillTV w sts = do
  tb <- textViewGetBuffer . castToTextView . head
        =<< containerGetChildren (castToContainer w)
  textBufferSetText tb (foldl (\b a -> b ++ a ++ "- ,\n") "" sts)

tmp :: [[Widget]] -> IO ()
tmp ws = do
  fillTV (last (ws !! 1)) ["диафрагма", "печень", "селезенка", "большой сальник", "желудок", "кишечник", "мочевой пузырь", "червеобразный отросток"]
