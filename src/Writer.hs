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

  -- \\\trowd\
  -- \\\cellx6221\
  -- \\\cellx6566\
  -- \\\cellx9922\
  -- \\\cell\
  -- \\\cell\
  -- \\\cell\
  -- \\\row\n\


writeHeaderTable :: Handle -> IO ()
writeHeaderTable out = appendRTFStringOrPara out $ Paragraph QLeft [RTFString Roman
  "\\fs18\

  \\\trowd\\trgaph58\
  \\\cellx6221\
  \\\cellx6566\
  \\\cellx9922\
  \{\\pard\\sl86\\b Областное бюджетное учреждение здравоохранения \\line\
  \«Курская городская клиническая больница скорой медицинской помощи»\\par}\\intbl\\cell\
  \\\cell\
  \{\\pard\\sl86\\qc Медицинская документация\\line\
  \Учетная форма № 013/у\\par}\\intbl\\cell\
  \\\row\n\

  \\\trowd\\trgaph58\
  \\\cellx6221\
  \\\cellx6566\
  \\\cellx9922\
  \Ул. Пирогова, 14. \\intbl\\cell\
  \\\cell\
  \\\sl86 Утверждена приказом Минздрава России от 6 июня 2013 г. № 354н \\intbl\\cell\
  \\\row\n"]

-- TODO search in Internet normal
monthIntToString :: String -> String
monthIntToString s = case s of
  "01" -> "января "
  "02" -> "февраля "
  "03" -> "марта "
  "04" -> "апреля "
  "05" -> "мая "
  "06" -> "июня "
  "07" -> "июля "
  "08" -> "августа "
  "09" -> "сентября "
  "10" -> "октября "
  "11" -> "ноября "
  "12" -> "декабря "
  _    -> undefined


writeText1 :: Handle -> [String] -> IO ()
writeText1 out (
                 numRep
               : dateRep
               : medRec
               : fio
               : org
               : dep
               : sex
               : dateBirth
               : age
               : dateDeath
               : bedDays
               : as) = do
  -- appendRTFStringOrPara out [Paragraph QLeft [RTFString Roman f] | f <- fs]
  -- let ft1 = map printf text1
  let (   Left orgT : Right medRecT : Left fioT : Left sexT
        : Left dateBirthT : Left ageT
        : Left dateDeathT : Left bedDaysT
        : qs) = text1
      (d1 : d2 : '.' : m1 : m2 : '.' : y) = dateRep
      upDateRep = printf "«%s» " ([d1, d2]) ++ (monthIntToString ([m1, m2])) ++ y -- каламбурное название
  appendRTFStringOrPara out $ [
                                Paragraph QCenter [RTFString Bold (header1 (read numRep :: Integer))]
                              , Paragraph QCenter [RTFString Roman upDateRep]
                              , Paragraph QJustify [RTFString Bold orgT, RTFString Roman (printf "%s; %s." org dep)]
                              , Paragraph QJustify [RTFString Bold (medRecT $ printf "%05d" (read medRec :: Integer))]
                              , Paragraph QJustify [RTFString Bold fioT, RTFString Roman fio]
                              , Paragraph QJustify [RTFString Bold sexT, RTFString Roman sex]
                              , Paragraph QJustify [ RTFString Bold dateBirthT, RTFString Roman (dateBirth ++ ";")
                                                , RTFString Bold ageT, RTFString Roman age]
                              , Paragraph QJustify [ RTFString Bold dateDeathT, RTFString Roman dateDeath
                                                , RTFString Bold bedDaysT, RTFString Roman bedDays]
                              ] ++ [Paragraph QJustify $ case q of
                                                    Left l  -> [RTFString Bold l, RTFString Roman a]
                                                    Right r -> [RTFString Bold (r a)]| (q, a) <- zip qs as]
  hPutStrLn out "\\line"
writeText1 _ _ = return ()

writeText2 :: Handle -> [[String]] -> IO ()
writeText2 out as = do
  appendRTFStringOrPara out $ Paragraph QCenter [RTFString Bold header2]
                            : [Paragraph QLeft [RTFString Bold q, RTFString Roman ("\\tab" ++ t a)] | (q, a, t) <- zip3 text2 as textInner2]

endRTF :: Handle -> IO ()
endRTF out = hPutStr out "}" >> hClose out
