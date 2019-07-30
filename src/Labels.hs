module Labels where
import           Data.List (findIndex, isSubsequenceOf)

labels1 :: [String]
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
        ]

search :: String -> Int
search s =
  let (Just i) = findIndex (\x -> s `isSubsequenceOf` x) labels1
  in i

medRecLabNum, fioLabNum, sexLabNum :: Int
dateRepLabNum,  dateBirthLabNum, dateDeathLabNum, dateRecLabNum :: Int
medRecLabNum = search "Медицинская карта №"
fioLabNum = search "Фамилия, имя, отчество"
sexLabNum = search "Пол"

dateRepLabNum = search "Дата протокола"
dateBirthLabNum = search "Дата рождения"
dateDeathLabNum = search "Дата смерти"
dateRecLabNum = search "Дата поступления"



-- fioLabNum :: Int
-- fioLabNum = search "Фамилия, имя, отчество"
