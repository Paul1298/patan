module Writer where
import           Data.List   (intercalate)
import           System.IO
import           Text.Printf (printf)

import           Text

data Format = Bold | Roman

instance Show Format where
  show Bold  = "\\b "
  show Roman = ""

data RTFString = RTFString { format  :: Format
                           , content :: String
                           }

instance Show RTFString where
  show (RTFString f c) = "{" ++  show f ++ c ++ "}"
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
  show (Paragraph a cs) = "{\\pard" ++ show a ++ "\n" ++ showList cs "" ++ "\n\\par}"
  showList ps = (++) $ intercalate "\n" (map show ps)


appendRTFStringOrPara :: (Show a) => Handle -> a -> IO ()
appendRTFStringOrPara out s = hPutStrLn out (show s)

pathFile :: String
pathFile = "test.rtf"

defsize :: Integer
defsize = 10

heading :: String
heading = "{\\rtf1\\ansi\\ansicpg1251\\deff0\\fs" ++ show (defsize * 2) ++ "{\\fonttbl {\\f0 Times New Roman;}}"

initRTF :: IO Handle
initRTF = do
  te <- mkTextEncoding "CP1251"
  out <- openFile pathFile WriteMode
  hSetEncoding out te
  startRTF out
  return out

startRTF :: Handle -> IO ()
startRTF out = hPutStrLn out heading

writeText1 :: Handle -> [String] -> IO ()
writeText1 out ( numRep
               : dateRep
               : medRec
               : fio
               : org
               : dep
               : as) = do
  appendRTFStringOrPara out [Paragraph QCenter [RTFString Bold (header1 ++ (printf "%03d" (read numRep :: Integer)) ++ ".")]]
  appendRTFStringOrPara out [Paragraph QCenter [RTFString Roman dateRep]]
  appendRTFStringOrPara out [Paragraph QLeft [RTFString Bold (head text1), RTFString Roman (org ++ "; " ++ dep ++ ".")]]
  appendRTFStringOrPara out [Paragraph QLeft [RTFString Bold q, RTFString Roman a] | (q, a) <- zip (tail text1) (medRec : fio : as)] -- q & a

endRTF :: Handle -> IO ()
endRTF out = hPutStr out "}"
