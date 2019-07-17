import qualified Data.Text                  as T
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Gdk.EventM
-- import           Graphics.UI.Gtk.Gdk.GC

textColumn :: ColumnId String T.Text
textColumn = makeColumnIdString 0

main = do
  initGUI
  window <- windowNew
  -- window `onDestroy` mainQuit
  windowSetDefaultSize window 800 600
  windowSetPosition window WinPosCenter

  store <- listStoreNew ["one", "two"]
  customStoreSetColumn store textColumn (\x -> T.pack $ "1") -- set the extraction function
  combo <- comboBoxNewWithModelAndEntry store
  comboBoxSetEntryTextColumn combo textColumn -- set which column should be used
  -- ren <- cellRendererTextNew
  -- cellLayoutPackEnd combo ren False
  -- cellLayoutSetAttributes combo ren store
  --   (\txt -> [cellText := "<-- your choice"])
  containerAdd window combo

  widgetShowAll window
  mainGUI
