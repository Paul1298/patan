{-# LANGUAGE ScopedTypeVariables #-}
module GUI.Fillings where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isDigit)
import           Data.Foldable          (foldlM)
import           Data.IORef
import           Data.List              (findIndices)
import           Graphics.UI.Gtk
import           Text.Printf

import           GUI.CommonGUI
import           Utils.Labels

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
    nextPos _ [i]      = i + 1
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
  buttonSetImage but =<< imageNewFromFile "resources/download.jpeg"
  widgetSetSizeRequest but 160 32

  void $ but `on` buttonActivated $ do
    widgetGrabFocus cal
    wis <- get cal widgetVisible
    if wis
    then widgetHide cal
    else widgetShow cal
  boxPackStart vbox but PackGrow 0
  boxPackStart vbox cal PackNatural 0
  boxPackEnd box vbox PackNatural 0

  void $ cal `on` focusOutEvent $ liftIO $ widgetHide cal >> return False
  return ()

fillWithCal :: [Entry] -> String -> IO ()
fillWithCal entries patt = do
  mapM_ (\x -> pattern x patt (findIndices (/= '_') patt)) entries
  mapM_ addCalendar entries

fillDates :: [Entry] -> IO ()
fillDates entries = fillWithCal entries "__.__.____ г."

fillDatesWithTime :: [Entry] -> IO ()
fillDatesWithTime entries = fillWithCal entries "__.__.____ г., __-__"

fillings1 :: [Entry] -> IO ()
fillings1 entries = do
  fillDates [ entries !! dateRepLabNum
            , entries !! dateBirthLabNum
            , entries !! datePsyLabNum
            ]
  fillDatesWithTime [
                      entries !! dateDeathLabNum
                    , entries !! dateRecLabNum
                    ]
  onlyInteger [ head entries
              , entries !! medRecLabNum
              , entries !! ageLabNum
              ]
  entrySetText (entries !! 0) "1"
  entrySetText (entries !! 4) "Областное бюджетное учреждение здравоохранения «Курская городская клиническая больница скорой медицинской помощи»"
  entrySetText (entries !! 21) "Указаны в посмертном клиническом эпикризе в истории болезни."

fillTV :: Widget -> [String] -> IO ()
fillTV w sts = do
  tb <- textViewGetBuffer =<< getTV w
  textBufferSetText tb (foldl1 (\a b -> a ++ " - ,\n" ++ b) sts ++ " - ")

partList :: [Int] -> Int -> [a] -> (a -> b -> c) -> [b -> c]
partList [] _ _ _                            = []
partList a@(i : is) j (x : xs) f | i == j    = f x : partList is (j + 1) xs f
                                 | otherwise = partList a (j + 1) xs f
partList _ _ _ _                             = []

fillings2 :: [[Widget]] -> IO ()
fillings2 ws = do
  sequence_ $ zipWith (\x s -> (flip entrySetText) s =<< getEntry x) (head ws) [" пола", " см.", " кг."]
  sequence_ $ zipWith fillItems ws tvInner2
  where
    fillItems items (is, ss) = sequence_ $ zipWith ($) (partList is 0 items fillTV) ss
