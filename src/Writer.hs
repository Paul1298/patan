module Writer where
import           Data.List (intercalate)
import           System.IO

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

data RTFString = RTFString { format  :: Format
                           , content :: String
                           }

instance Show RTFString where
  show (RTFString f c) = show f ++ c
  showList cs = (++) $ intercalate "\n" (map show cs)

data Paragraph = Paragraph { alignment :: Quadding
                           , contents  :: [RTFString]
                           }

instance Show Paragraph where
  show (Paragraph a cs) = "{\\pard" ++ show a ++ "\n" ++ showList cs "" ++ "\n\\par}\n"
  showList ps = (++) $ intercalate "\n" (map show ps)

appendRTFStringOrPara :: (Show a) => a -> IO ()
appendRTFStringOrPara s = appendFile pathFile (show s)

defsize = 10

startRTF = "{\\rtf1\\ansi\\deff0\\fs" ++ show (defsize * 2) ++ "{\\fonttbl {\\f0 Times New Roman;}}\n"

writeRTF :: IO ()
writeRTF = do
  writeFile pathFile startRTF
  textFile <- openFile "text.txt" ReadMode
  hSetEncoding textFile latin1
  text <- hGetContents textFile
  headersFile <- openFile "headers.txt" ReadMode
  hSetEncoding textFile latin1
  headers <- hGetContents headersFile
  -- headers <- readFile "headers.txt"
  let templateFields = lines text
  let f = [Paragraph QJustify [RTFString Bold x] | x <- lines text]
  let h = [Paragraph QCenter [RTFString Bold h] | h <- lines headers]
  appendRTFStringOrPara h
  appendRTFStringOrPara f
  -- mapM appendRTFStringOrPara f
  appendFile pathFile "}"
