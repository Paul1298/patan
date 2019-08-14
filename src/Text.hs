module Text where

import           Text.Printf (PrintfType, printf)


header1 :: Integer -> String
header1 = printf "ПРОТОКОЛ\\line патологоанатомического вскрытия № %03d."

text1 :: [Either String (String -> String)]
text1 = [
          Left "1. Наименование медицинской организации и отделения, в котором наблюдался и умер пациент: "
        , Right $ printf "2. Медицинская карта стационарного пациента № %s"
        , Left "3. Фамилия, имя, отчество умершего: "
        , Left "4. Пол: "
        , Left "5. Дата рождения: "
        , Left "\\tab\\tabПолных лет – "
        , Left "6. Дата смерти: "
        , Left "\\tab\\tabПроведено койко-дней – "
        , Left "7. Место жительства умершего: "
        , Left "8. Местность: "
        , Left "9. Семейное положение: "
        , Left "10. Образование: "
        , Left "11. Занятость: "
        , Left "12. Дата поступления в медицинскую организацию, в которой наблюдался и умер пациент: "
        , Left "13. Доставлен в медицинскую организацию, в которой наблюдался и умер пациент через "
        , Left "14. Фамилия, имя, отчество лечащего врача: "
        , Left "15. Лечащий врач (заведующий отделением) присутствовал на патологоанатомическом вскрытии "
        , Left "16. Дата проведения патологоанатомического вскрытия: "
        , Left "17. Основные клинические данные: "
        , Right $ printf "18. Заключительный клинический диагноз: \\tab\\tab\\tabкод по МКБ-Х (%s)"
        , Left "Основное заболевание: "
        , Left "Осложнения основного заболевания: "
        , Left "Сопутствующие заболевания: "
        ]

header2 :: String
header2 = "{\\ulПАТОЛОГОАНАТОМИЧЕСКОЕ ВСКРЫТИЕ}"

text2 :: [String]
text2 = [
          "19. Наружный осмотр тела:\\line"
        , "20. Брюшная полость.\\line"
        , "21. Грудная полость.\\line"
        , "22. Полость черепа.\\line"
        , "23. Органы кровообращения\\line"
        , "24. Органы дыхания\\line"
        , "25. Органы пищеварения\\line"
        , "26. Органы мочеполовой системы\\line"
        , "27. Органы кроветворения\\line"
        , "28. Эндокринные железы\\line"
        , "29. Костно-мышечная система\\line"
        ]

printfList_ :: PrintfType t => String -> [String] -> Int -> t
printfList_ string list n | n == 0 = printf string (list !! 0)
                          | otherwise = (printfList_ string list (n - 1)) (list !! n)

printfList :: String -> [String] -> String
printfList string list = (printfList_ string list (length list - 1)) :: String

textInner2 :: [[String] -> String]
textInner2 = map printfList
             [
              "Труп %s пола. Длина тела %s см. Масса тела %s кг.\n\
               \Телосложение %s, состояние питания %s."
             ]
