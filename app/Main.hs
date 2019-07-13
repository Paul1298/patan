module Main where
import           GUI
import           Template
import           Writer

import           System.IO

main :: IO ()
main = do
  te <- mkTextEncoding "CP1251"
  out <- openFile pathFile WriteMode
  hSetEncoding out te
  writeRTF out
  startGUI out
  hPutStr out "}"
  hClose out
