{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module  GUI.CommonGUI where

import           Control.Monad          (void)
import           Control.Monad.Extra    (whenJust)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Graphics.UI.Gtk
import           Prelude                hiding (drop, length)

getTV :: WidgetClass w => w -> IO TextView
getTV w = castToTextView . head <$> containerGetChildren (castToContainer w)

getEntry :: WidgetClass w => w -> IO Entry
getEntry w = do
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
focLabel w = widgetModifyBg w StateNormal (Color 16640 26880 57600)
unfocLabel :: Widget -> IO ()
unfocLabel w = widgetRestoreBg w StateNormal


colorOnFocus :: Widget -> Widget -> IO ()
colorOnFocus wid lab = do
  void $ wid `on` focusInEvent $ liftIO $ focLabel lab >> return False
  void $ wid `on` focusOutEvent $ liftIO $ unfocLabel lab >> return False
  return ()

textColumn :: ColumnId row Text
textColumn = makeColumnIdString 0

initGrid :: Int -> [Text] -> IO [Maybe [Text]] -> IO (Grid, [Entry], [Widget])
initGrid n labelsText initdefs = do
  grid <- gridNew
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

  defs <- initdefs
  stores <- sequence [case def of
                        Just d  -> do
                                   l <- listStoreNew d
                                   return $ Just l
                        Nothing -> return Nothing| def <- defs]
  sequence_ [whenJust store $ \st -> customStoreSetColumn st textColumn id | store <- stores ] -- set the extraction function

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
                 frame <- frameNew
                 -- set frame [widgetMarginRight := 10]
                 containerAdd frame =<< textViewNew
                 return $ castToWidget frame
    | store <- stores]
  sequence_ [gridAttach grid c 1 i 2 1 | (c, i) <- zip combos [0..n - 1]]

  names <- mapM widgetGetName combos
  let ncs = zip names combos

  entries <- mapM (getEntry . snd) $ filter ((/= ("GtkFrame" :: String)) . fst) ncs

  sequence_ [do
             len <- listStoreGetSize st
             if (len /= 0)
             then do
               ec <- entryCompletionNew
               set ec [ entryCompletionModel      := Just st
                      , entryCompletionTextColumn := textColumn]
               entrySetCompletion en ec
             else return () | (en, st) <- zip entries (catMaybes stores)]

  painters <- mapM (\(name, w) -> case name of
                                "GtkFrame" -> castToWidget <$> getTV w
                                _          -> castToWidget <$> getEntry w) ncs

  sequence_ [colorOnFocus wid (castToWidget lab) | (wid, lab) <- zip painters labels]

  return (grid, entries, combos)

createMenuBar :: [([Char], [([Char], Maybe (IO ()))])] -> IO MenuBar
createMenuBar descr
    = do bar <- menuBarNew
         mapM_ (createMenu bar) descr
         return bar
    where
      createMenu bar (name,items)
          = do menu <- menuNew
               item <- menuItemNewWithLabelOrMnemonic name
               menuItemSetSubmenu item menu
               menuShellAppend bar item
               mapM_ (createMenuItem menu) items
      createMenuItem menu (name,action)
          = do item <- menuItemNewWithLabelOrMnemonic name
               menuShellAppend menu item
               case action of
                 Just act -> on item menuItemActivate act
                 Nothing  -> on item menuItemActivate (return ())
      menuItemNewWithLabelOrMnemonic name
          | elem '_' name = menuItemNewWithMnemonic name
          | otherwise     = menuItemNewWithLabel name

menuBarDescr :: [([Char], [([Char], Maybe (IO ()))])]
menuBarDescr
    = [ ("_File", [ ("Open", Nothing)
                  , ("Save", Nothing)
                  , ("_Quit", Just mainQuit)
                  ]
        )
      , ("Help",  [ ("_Help", Nothing)
                  ]
        )
      -- , ("Help",  [ ("_Help", Nothing)
      --             ]
      --   )
      ]

-- saveAction :: IO ()
