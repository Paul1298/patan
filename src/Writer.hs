module Writer where
import           Data.List (intercalate)
import           System.IO

-- import           GUI       (startGUI)
import           Headers
import           Text

data Format = Bold | Roman

instance Show Format where
  show Bold  = "\\b "
  show Roman = ""

data RTFString = RTFString { format  :: Format
                           , content :: String
                           }

instance Show RTFString where
  show (RTFString f c) = show f ++ c
  showList cs = (++) $ intercalate "\n" (map show cs)

data Quadding = QLeft | QCenter | QRight | QJustify

instance Show Quadding where
  show QLeft    = "\\ql "
  show QCenter  = "\\qc "
  show QRight   = "\\qr "
  show QJustify = "\\qj "

data Paragraph = Paragraph { alignment :: Quadding
                           , contents  :: [RTFString]
                           }

instance Show Paragraph where
  show (Paragraph a cs) = "{\\pard" ++ show a ++ "\n" ++ showList cs "" ++ "\n\\par}\n"
  showList ps = (++) $ intercalate "\n" (map show ps)

pathFile = "test.rtf"

appendRTFStringOrPara :: (Show a) => Handle -> a -> IO ()
appendRTFStringOrPara out s = hPutStrLn out (show s)

defsize = 10

startRTF = "{\\rtf1\\ansi\\ansicpg1251\\deff0\\fs" ++ show (defsize * 2) ++ "{\\fonttbl {\\f0 Times New Roman;}}"

writeRTF :: Handle -> IO ()
writeRTF out = do
  hPutStrLn out startRTF
  let h = [Paragraph QCenter [RTFString Bold h] | h <- headers]
  appendRTFStringOrPara out h
