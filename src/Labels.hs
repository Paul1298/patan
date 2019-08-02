{-# LANGUAGE OverloadedStrings #-}
module Labels where
import           Data.List (findIndex, isSubsequenceOf)
import           Data.Text (Text, unpack)

labels1 :: [Text]
labels1 = [
          "№ протокола"
        , "Дата протокола"
        , "Медицинская карта №"
        , "Фамилия, имя, отчество"
        , "Медицинская организация"
        , "Отделение"
        , "Пол"
        , "Дата рождения"
        , "Дата смерти"
        , "Место жительства"
        , "Местность"
        , "Семейное положение"
        , "Образование"
        , "Занятость"
        , "Дата поступления"
        , "Доставлен в медицинскую организацию"
        , "ФИО лечащего врача"
        , "Леч. врач (зав. отделением)\nприсутствовал на вскрытии?\n"
        , "Дата вскрытия"
        , "Основные клинические данные"
        , "Заключительный клинический диагноз"
        , "Основное заболевание"
        , "Осложнения основного заболевания"
        , "Сопутствующие заболевания"
        ]

-- TODO
-- isSubsequenceOf :: Text -> Text -> Bool
-- isSubsequenceOf empty _                    = False
-- isSubsequenceOf _     empty                = True
-- isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
--                                | otherwise = isSubsequenceOf a b

search :: Text -> Int
search s =
  let (Just i) = findIndex (\x -> (unpack s) `isSubsequenceOf` (unpack x)) labels1
  in i

medRecLabNum, fioLabNum, sexLabNum :: Int
medRecLabNum = search "Медицинская карта №"
fioLabNum    = search "Фамилия, имя, отчество"
sexLabNum    = search "Пол"

dateRepLabNum,  dateBirthLabNum, dateDeathLabNum, dateRecLabNum, datePsyLabNum :: Int
dateRepLabNum   = search "Дата протокола"
dateBirthLabNum = search "Дата рождения"
dateDeathLabNum = search "Дата смерти"
dateRecLabNum   = search "Дата поступления"
datePsyLabNum   = search "Дата вскрытия"

labels2 :: [Text]
labels2 = [
            "Наружный осмотр тела"
          , "Брюшная полость"
          , "Грудная полость"
          , "Полость черепа"
          , "Органы кровообращения"
          , "Органы дыхания"
          , "Органы пищеварения"
          , "Органы мочеполовой системы"
          , "Органы кроветворения"
          , "Эндокринные железы"
          , "Костно-мышечная система"
          , "Для гистологического исследования взяты"
          , "Для дополнительных исследований (указать каких) взяты"
          ]

labelsInner2 :: [[Text]]
labelsInner2 =
  let
   i19 =
  in [
       "Наружный осмотр тела"
     , "Брюшная полость"
     , "Грудная полость"
     , "Полость черепа"
     , "Органы кровообращения"
     , "Органы дыхания"
     , "Органы пищеварения"
     , "Органы мочеполовой системы"
     , "Органы кроветворения"
     , "Эндокринные железы"
     , "Костно-мышечная система"
     , "Для гистологического исследования взяты"
     , "Для дополнительных исследований (указать каких) взяты"
     ]
