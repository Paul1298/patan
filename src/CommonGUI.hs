{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CommonGUI where

import           Control.Monad          (filterM, join, void)
import           Control.Monad.Extra    (fromMaybeM, whenJust)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Graphics.UI.Gtk
import           Prelude                hiding (drop, length)

getEntry :: WidgetClass w => w -> IO Entry
getEntry w = do
  -- TODO по-нормальному сделать
  name <- widgetGetName w
  case name of
    ("GtkComboBox" :: String) ->
                            do
                            Just e <- binGetChild (castToComboBox w)
                            return $ castToEntry e
    ("GtkHBox" :: String)     ->
                            do
                            let e = head <$> containerGetChildren (castToContainer w)
                            castToEntry <$> e
    _                         -> undefined

focLabel :: Widget -> IO ()
focLabel w = widgetModifyBg w StateNormal (Color 0 34000 0)
unfocLabel :: Widget -> IO ()
unfocLabel w = widgetRestoreBg w StateNormal


colorOnFocus :: IO Widget -> Widget -> IO ()
colorOnFocus mw wid = do
  w <- mw
  void $ wid `on` focusInEvent $ liftIO $ focLabel w >> return False
  void $ wid `on` focusOutEvent $ liftIO $ unfocLabel w >> return False
  return ()

textColumn :: ColumnId row Text
textColumn = makeColumnIdString 0


initGrid :: Int -> [Text] -> IO [Maybe [Text]] -> IO (Grid, [Entry])
initGrid n labelsText initdefs = do
  grid <- gridNew
  -- gridSetRowHomogeneous grid True -- rows same height
  gridSetColumnHomogeneous grid True
  gridSetRowSpacing grid 4
  gridSetColumnSpacing grid 1
  -- set grid [ containerBorderWidth := 2 ]

  -- labels
  labels <- sequence [labelNew $ Just l | l <- labelsText] -- init labels
  sequence_ [do
              labelSetEllipsize l EllipsizeEnd
              labelSetMaxWidthChars l 140
              miscSetAlignment l 0 0.5
              miscSetPadding l 20 0
              gridAttach grid l 0 i 1 1 | (l, i) <- zip labels [0..n - 1]] --attach them
  widgetSetCanFocus (labels !! 0) True
  widgetGrabFocus (labels !! 0)

  -- entries with comboBox
  defs <- initdefs --[Maybe [Text]]
  stores <- sequence [case def of
                        Just d  -> do
                                   l <- listStoreNew d
                                   return $ Just l
                        Nothing -> return Nothing| def <- defs]
  sequence_ [whenJust store $ \st -> customStoreSetColumn st textColumn id | store <- stores ] -- set the extraction function

  -- combos
  combos <- sequence
    [case store of
      Just st -> do
                 len <- listStoreGetSize st
                 if (len /= 0)
                 then do
                   c <- comboBoxNewWithModelAndEntry st
                   comboBoxSetEntryTextColumn c textColumn
                   return $ castToWidget c
                 else do
                   box <- hBoxNew False 0
                   en <- entryNew
                   boxPackStart box en PackGrow 0
                   return $ castToWidget box
      Nothing -> do
                 tv <- frameNew
                 containerAdd tv =<< textViewNew
                 return $ castToWidget tv
    | store <- stores]
  -- putStrLn . show =<< (mapM widgetGetName combos :: IO [String])
  sequence_ [gridAttach grid c 1 i 2 1 | (c, i) <- zip combos [0..n - 1]]
  -- widgetGetName (combos !! 0) >>= putStrLn

  -- TODO
  entries <- join $ mapM getEntry <$> (filterM (\x -> (/= "GtkFrame") <$> (widgetGetName x :: IO String)) combos)

  -- entry-completion
  sequence_ [do
             len <- listStoreGetSize st
             if (len /= 0)
             then do
               ec <- entryCompletionNew
               set ec [ entryCompletionModel      := Just st
                      , entryCompletionTextColumn := textColumn]
               entrySetCompletion en ec
             else return () | (en, st) <- zip entries (catMaybes stores)]

  sequence_ [colorOnFocus (fromMaybeM undefined $ gridGetChildAt grid 0 i) (castToWidget wid) | (wid, i) <- zip entries [0..n - 1]]

  return (grid, entries)
