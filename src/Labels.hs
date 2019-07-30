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
