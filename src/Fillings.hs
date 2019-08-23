{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fillings where

import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isDigit)
import           Data.IORef
import           Data.Text              (Text, drop, length, take, unpack)
import           Graphics.UI.Gtk
import           Text.Printf

import           Prelude                hiding (drop, length, take)

import           Labels

onlyInteger :: [Entry] -> IO ()
onlyInteger entries = do
  mapM_ only entries
  where
    only :: Entry -> IO ()
    only entry = do
      idIRef <- newIORef undefined
      idI <- entry `on` insertText $ \(str :: Text) pos -> do
        idI <- readIORef idIRef
        signalBlock idI
        pos' <- newIORef pos
        if (all isDigit $ unpack str)
        then editableInsertText entry str pos >>= writeIORef pos'
        else return ()

        signalUnblock idI
        stopInsertText idI
        readIORef pos'
      writeIORef idIRef idI

fillDates :: [Entry] -> IO ()
fillDates entries = do
  mapM_ pattern entries
  mapM_ addCalendar entries
  where
    pattern :: Entry -> IO ()
    pattern entry = do
      let def = "__.__.____ г." :: Text
      entrySetText entry def
      let n = length def

      editWrite <- newIORef True
      editRead <- newIORef True

      idIRef <- newIORef undefined
      idI <- entry `on` insertText $ \(str :: Text) pos -> do
        idI <- readIORef idIRef
        signalBlock idI
        pos' <- newIORef pos
        flag <- readIORef editWrite
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
            _ <- editableInsertText entry substr startPos
            writeIORef editWrite True
        else return ()
        signalUnblock idD
        stopDeleteText idD
      writeIORef idDRef idD

    addCalendar :: Entry -> IO ()
    addCalendar entry = do

      cal <- calendarNew

      _ <- onDaySelected cal $ do
        (y, m, d) <- calendarGetDate cal
        let date = (printf "%02d" d) ++ "." ++ (printf "%02d" (m + 1)) ++ "." ++ (printf "%04d" y)
        entrySetText entry date

      (_, _, today) <- calendarGetDate cal
      calendarSelectDay cal today

      Just box <- fmap castToHBox <$> widgetGetParent entry
      vbox <- vBoxNew False 1
      containerSetResizeMode vbox ResizeQueue
      _ <- vbox `on` showSignal $ widgetHide cal

      but    <- buttonNew
      image <- imageNewFromFile "download.jpeg"
      containerAdd but image
      widgetSetSizeRequest but 160 10


      _ <- but `on` buttonActivated $ do
        widgetGrabFocus cal
        -- TODO add color label
        wis <- get cal widgetVisible
        if wis
        then widgetHide cal
        else widgetShow cal
      boxPackStart vbox but PackGrow 0
      boxPackStart vbox cal PackNatural 0
      boxPackEnd box vbox PackNatural 0

      _ <- cal `on` focusOutEvent $ liftIO $ widgetHide cal >> return False
      return ()

fillings1 :: [Entry] -> IO ()
fillings1 entries = do
  fillDates [ entries !! dateRepLabNum
            , entries !! dateBirthLabNum
            , entries !! dateDeathLabNum
            , entries !! dateRecLabNum
            , entries !! datePsyLabNum
            ]
  onlyInteger [ head entries
              , entries !! medRecLabNum
              , entries !! ageLabNum
              , entries !! 10
              ]
  entrySetText (entries !! 0) ("1" :: Text)
  entrySetText (entries !! 4) ("Областное бюджетное учреждение здравоохранения «Курская городская клиническая больница скорой медицинской помощи»" :: Text)
  entrySetText (entries !! 21) ("Указаны в посмертном клиническом эпикризе в истории болезни." :: Text)

  -- nb <- buttonNewWithLabel ("1" :: Text)
  -- containerAdd (entries !! dateRepLabNum) nb
