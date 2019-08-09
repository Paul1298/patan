module Writer where
import           Data.List   (intercalate)
import           System.IO
import           Text.Printf (printf)

import           Text

initRTF :: IO Handle
initRTF = do
  te <- mkTextEncoding "CP1251"
  out <- openFile pathFile WriteMode
  hSetEncoding out te
  startRTF out
  return out

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
  show QLeft    = "\\ql"
  show QCenter  = "\\qc"
  show QRight   = "\\qr"
  show QJustify = "\\qj"

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

pageSize :: String
pageSize = "\\paperw11909\\paperh16837\\margl1138\\margr850\\margt562\\margb562"

startRTF :: Handle -> IO ()
startRTF out = do
  hPutStrLn out heading
  hPutStrLn out pageSize

  -- \trowd
  -- \cellx6221
  -- \cellx6566
  -- \cellx9922
  -- \cell
  -- \cell
  -- \cell
  -- \row
  --
  -- \trowd
  -- \cellx6221
  -- \cellx6566
  -- \cellx9922
  -- Областное бюджетное учреждение здравоохранения
  -- «Курская городская клиническая больница скорой медицинской помощи»
  -- \intbl\cell
  -- \cell
  -- Медицинская документация
  -- Учетная форма № 013/у\intbl\cell
  -- \row
  --
  -- \trowd
  -- \cellx6221
  -- \cellx6566
  -- \cellx9922
  -- \cell
  -- \cell
  -- Утверждена приказом Минздрава России от 6 июня 2013 г. № 354н \intbl\cell
  -- \row


writeText1 :: Handle -> [String] -> IO ()
writeText1 out fs@(
                 numRep
               : dateRep
               : medRec
               : fio
               : org
               : dep
               : as) = do
  -- appendRTFStringOrPara out [Paragraph QLeft [RTFString Roman f] | f <- fs]
  let (orgT : medRecT : ts) = text1
  appendRTFStringOrPara out $ [
                                Paragraph QCenter [RTFString Bold (header1 ++ (printf "%03d" (read numRep :: Integer)) ++ ".")]
                              , Paragraph QCenter [RTFString Roman dateRep]
                              , Paragraph QLeft [RTFString Bold orgT, RTFString Roman (org ++ "; " ++ dep ++ ".")]
                              , Paragraph QLeft [RTFString Bold (medRecT ++ printf "%05d" (read medRec :: Integer))]
                              ] ++ [Paragraph QLeft [RTFString Bold q, RTFString Roman a] | (q, a) <- zip ts (fio : as)]
  hPutStrLn out "\\line"

writeText2 :: Handle -> [String] -> IO ()
writeText2 out as = do
  appendRTFStringOrPara out $ Paragraph QCenter [RTFString Bold header2]
                            : [Paragraph QLeft [RTFString Bold q, RTFString Roman a] | (q, a) <- zip text2 as]

endRTF :: Handle -> IO ()
endRTF out = hPutStr out "}"
