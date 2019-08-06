{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CommonGUI where

import           Data.Text       (Text)
import           Graphics.UI.Gtk
import           Prelude         hiding (drop, length, take)

getEntry :: WidgetClass w => w -> IO Entry
getEntry w = do
  -- TODO
  name <- widgetGetName w
  case name of
    ("GtkComboBox" :: String) -> do
                            Just e <- binGetChild (castToComboBox w)
                            return $ castToEntry e
    ("GtkHBox" :: String)      -> do
                            let e = head <$> containerGetChildren (castToContainer w)
                            castToEntry <$> e



initGrid :: Int -> [Text] -> IO [[Text]] -> IO (Grid, [Entry])
initGrid n labelsText initdefs = do
  grid <- gridNew
  -- gridSetRowHomogeneous grid True -- rows same height
  gridSetColumnHomogeneous grid True
  gridSetRowSpacing grid 2

  -- labels
  labels <- sequence [labelNew $ Just l | l <- labelsText] -- init labels
  sequence_ [miscSetAlignment l 0 0 | l <- labels] -- left alignment labels
  sequence_ [gridAttach grid l 0 i 1 1 | (l, i) <- zip labels [0..n - 1]] --attach them
  -- sequence_ [labelSetSingleLineMode l False | l <- labels] -- sets the desired width in character
  widgetSetCanFocus (labels !! 0) True
  widgetGrabFocus (labels !! 0)

  let textColumn = makeColumnIdString 0

  -- entries with comboBox
  defs <- initdefs
  stores <- sequence [listStoreNew def | def <- defs]
  sequence_ [customStoreSetColumn store textColumn id | store <- stores ] -- set the extraction function

  -- combos
  combos <- sequence [do
                      len <- listStoreGetSize store
                      if (len /= 0)
                      then do
                        c <- comboBoxNewWithModelAndEntry store
                        comboBoxSetEntryTextColumn c textColumn
                        return $ castToWidget c
                      else do
                        box <- hBoxNew False 0
                        en <- entryNew
                        boxPackEnd box en PackGrow 0
                        -- widgetGetName box >>= putStrLn
                        return $ castToWidget box | store <- stores ]
  sequence_ [gridAttach grid c 1 i 1 1 | (c, i) <- zip combos [0..n - 1]]
  -- widgetGetName (combos !! 0) >>= putStrLn

  -- entries
  entries <- mapM getEntry combos

  -- entry-completion
  ecompls <- sequence $ replicate n entryCompletionNew
  sequence_ [set ec [ entryCompletionModel            := Just st
                    , entryCompletionMinimumKeyLength := 0
                    , entryCompletionTextColumn       := textColumn] | (ec, st) <- zip ecompls stores]

  -- sequence_ [entryCompletionSetMinimumKeyLength ec 0 | ec <- ecompls]
  sequence_ [entrySetCompletion e ec | (e, ec) <- zip entries ecompls]

  return (grid, entries)
