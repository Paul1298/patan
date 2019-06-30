module Writer where
import Data.List(intercalate)

pathFile = "test.rtf"

data Format = Bold | Roman

instance Show Format where
  show Bold  = "\\b "
  show Roman = ""

data Quadding = QLeft | QCenter | QRight | QJustify

instance Show Quadding where
  show QLeft    = "\\ql "
  show QCenter  = "\\qc "
  show QRight   = "\\qr "
  show QJustify = "\\qj "

data RTFString = RTFString { format :: Format
                           , content :: String
                           }

instance Show RTFString where
  show (RTFString f c) = show f ++ show c
  showList cs = (++) $ intercalate "\n" (map show cs)

data Paragraph = Paragraph { alignment :: Quadding
                           , contents :: [RTFString]
                           }

instance Show Paragraph where
  show (Paragraph a cs) = "{\\pard" ++ show a ++ "\n" ++ showList cs "" ++ "\n\\par}\n"

appendRTFStringOrPara :: (Show a) => a -> IO ()
appendRTFStringOrPara s = appendFile pathFile (show s)

startRTF = "{\\rtf1\\ansi\\deff0\\fs22 {\\fonttbl {\\f0 Times New Roman;}}\n"

test = Paragraph QJustify [RTFString Bold "qwdqwdqwd"]

writeRTF :: IO ()
writeRTF = do
  file <- readFile "text.txt"
  let templateFields = lines file
  let f = [RTFString Bold x | x <- templateFields]
  writeFile pathFile startRTF
  mapM appendRTFStringOrPara f
  appendFile pathFile "}"
