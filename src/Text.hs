module Text where

import           Data.Text   (unpack)
import           Text.Printf (PrintfType, printf)

import           Labels

header1 :: Integer -> String
header1 = printf "ПРОТОКОЛ\\line патологоанатомического вскрытия № %03d."

text1 :: [Either String (String -> String)]
text1 = [
          Left "1. Наименование медицинской организации и отделения, в котором наблюдался и умер пациент: "
        , Right $ printf "2. Медицинская карта стационарного пациента № %s"
        , Left "3. Фамилия, имя, отчество умершего: "
        , Left "4. Пол: "
        , Left "5. Дата рождения: "
        , Left "\\tab\\tab Полных лет – "
        , Left "6. Дата смерти: "
        , Left "\\tab Проведено койко-дней – "
        , Left "7. Место жительства умершего: "
        , Left "8. Местность: "
        , Left "9. Семейное положение: "
        , Left "10. Образование: "
        , Left "11. Занятость: "
        , Left "12. Дата поступления в медицинскую организацию, в которой наблюдался и умер пациент: "
        , Right $ printf "13. Доставлен в медицинскую организацию, в которой наблюдался и умер пациент через %s после начала заболевания"
        , Left "14. Фамилия, имя, отчество лечащего врача: "
        , Left "15. Лечащий врач (заведующий отделением) присутствовал на патологоанатомическом вскрытии "
        , Left "16. Дата проведения патологоанатомического вскрытия: "
        , Left "17. Основные клинические данные: "
        , Right $ printf "18. Заключительный клинический диагноз:\\tab\\tab\\tab код по МКБ-Х (%s)"
        , Left "Основное заболевание:\\par"
        , Left "Осложнения основного заболевания:\\par"
        , Left "Сопутствующие заболевания:\\par"
        ]

header2 :: String
header2 = "{\\ulПАТОЛОГОАНАТОМИЧЕСКОЕ ВСКРЫТИЕ}"

text2 :: [String]
text2 = map (++ "\\par ") [
          "19. Наружный осмотр тела:"
        , "20. Брюшная полость."
        , "21. Грудная полость."
        , "22. Полость черепа."
        , "23. Органы кровообращения."
        , "24. Органы дыхания."
        , "25. Органы пищеварения."
        , "26. Органы мочеполовой системы:"
        , "27. Органы кроветворения:"
        , "28. Эндокринные железы:"
        , "29. Костно-мышечная система:"
        ]

printfList_ :: PrintfType t => String -> [String] -> Int -> t
printfList_ string list n | n == 0    = printf string (list !! 0)
                          | otherwise = (printfList_ string list (n - 1)) (list !! n)

printfList :: String -> [String] -> String
printfList string list = (printfList_ string list (length list - 1)) :: String

textInner2 :: [[String] -> String]
textInner2 = map (printfList . foldl (\b a -> b ++ a ++ " %s, ") "" . fmap unpack) labelsInner2
