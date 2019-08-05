{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fillings where

import           Data.Char       (isDigit)
import           Data.IORef
import           Data.Text       (Text, drop, length, take, unpack)
import           Graphics.UI.Gtk

import           Prelude         hiding (drop, length, take)

import           Labels

fillDates :: [Entry] -> IO ()
fillDates entries = do
  mapM_ fill entries
  where
    fill :: Entry -> IO ()
    fill entry = do
      let def = "__.__.____" :: Text
      let n = length def

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
            if (pos >= n)
            then writeIORef pos' n
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
        let tmp = if (endPos < 0) then n else endPos
        -- entryGetText entry >>= putStrLn
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

fillings1 :: [Entry] -> IO ()
fillings1 entries = do
  fillDates [ entries !! dateRepLabNum
            , entries !! dateBirthLabNum
            , entries !! dateDeathLabNum
            , entries !! dateRecLabNum
            , entries !! datePsyLabNum
            ]
  entrySetText (entries !! 0) ("1" :: Text)
  entrySetText (entries !! 4) ("Областное бюджетное учреждение здравоохранения «Курская городская клиническая больница скорой медицинской помощи»" :: Text)
  entrySetText (entries !! 20) ("Указаны в посмертном клиническом эпикризе в истории болезни." :: Text)

  -- nb <- buttonNewWithLabel ("1" :: Text)
  -- containerAdd (entries !! dateRepLabNum) nb
