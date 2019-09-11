{-# LANGUAGE OverloadedStrings #-}
module Utils.Labels where
import           Data.List (findIndex, isSubsequenceOf)
import           Data.Text (Text, unpack)

labels1 :: [Text]
labels1 = [
            "№ протокола"
          , "Дата составления протокола"
          , "Медицинская карта №"
          , "Фамилия, имя, отчество"
          , "Название мед. организации"
          , "Отделение"
          , "Пол"
          , "Дата рождения"
          , "Полных лет"
          , "Дата смерти"
          , "Проведено койко-дней"
          , "Место жительства"
          , "Местность"
          , "Семейное положение"
          , "Образование"
          , "Занятость"
          , "Дата поступления"
          , "Когда доставлен"
          , "ФИО лечащего врача"
          , "Леч. врач (зав. отделением)\nприсутствовал на вскрытии?"
          , "Дата вскрытия"
          , "Основные клинические данные"
          , "Заключительный диагноз (код по МКБ-Х)"
          , "Основное заболевание"
          , "Осложнения основного заболевания"
          , "Сопутствующие заболевания"
          ]

-- TODO for Data.Text
-- isSubsequenceOf :: Text -> Text -> Bool
-- isSubsequenceOf empty _                    = False
-- isSubsequenceOf _     empty                = True
-- isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
--                                | otherwise = isSubsequenceOf a b

search :: Text -> Int
search s =
  let (Just i) = findIndex (\x -> (unpack s) `isSubsequenceOf` (unpack x)) labels1
  in i

medRecLabNum, fioLabNum, sexLabNum, ageLabNum :: Int
medRecLabNum = search "Медицинская карта №"
fioLabNum    = search "Фамилия, имя, отчество"
sexLabNum    = search "Пол"
ageLabNum    = search "Полных лет"

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
          ]

tvInner2 :: [([Int], [[String]])]
tvInner2 = [
             ([6, 11], [["головы", "шеи", "груди", "туловища", "конечностей"]
                       ,["длина разреза (см)", "характер операционной раны", "швы", "выделения из раны"]])
           , ([4], [["диафрагма", "печень", "селезенка", "большой сальник", "желудок", "кишечник", "мочевой пузырь", "червеобразный отросток"]])
           , ([2, 3], [["форма", "расположение"]
                      ,["расположение", "величина"]])
           , ([2, 3], [["мягкая", "паутинная", "твердая", "кровенаполнение сосудов (?)"]
                      ,["объем", "масса", "консистенция"]])
           , ([0, 5, 6], [["размеры", "масса", "эпикард", "перикард"]
                         ,["правого", "левого", "межжелудочковой перегородки (?)"]
                         ,["эндокард", "клапаны сердца", "венечные артерии"]])
           , ([4, 10], [["размеры правого", "размеры левого", "масса", "форма", "плевральные листки"]
                       ,["сосудов", "бронхов"]])
           , ([4, 5, 6, 8], [["тонкая", "толстая"]
                            ,["размеры", "масса", "форма", "консистенция", "окраска", "характер поверхности", "вид на разрезе"]
                            ,["размеры", "консистенция стенки", "цвет желчи"]
                            ,["масса", "размеры", "цвет", "консистенция", "рисунок ткани на разрезе"]])
           , ([0], [["размеры", "масса", "консистенция", "характер поверхности", "вид на разрезе", "толщина коркового вещества", "слизистая оболочка лоханок"]])
           , ([0, 1], [["размеры", "масса", "консистенция", "вид снаружи", "вид на разрезе"]
                      ,["брыжейки", "средостения", "шеи"]])
           , ([0, 1], [["размеры", "консистенция", "вид снаружи", "вид на разрезе"]
                      ,["форма", "цвет на разрезе", "рисунок ткани"]])
           ]


labelsInner2 :: [[Text]]
labelsInner2 = [
                 [ -- 19. Наружный осмотр тела
                   "Труп"
                 , "Длина тела"
                 , "Масса тела"
                 , "Телосложение"
                 , "Cостояние питания"
                 , "Cостояние мышечной и костной систем"
                 , "Кожный покров:"
                 , "Трупные пятна и их расположение"
                 , "Выраженность и распространенность трупного окоченения"
                 , "Состояние естественных отверстий"
                 , "Наружные половые органы сформированы (?)"
                 , "Операционные раны:"
                 , "Следы инъекций и изменения в их зоне"
                 ]
               , [ -- 20. Брюшная полость
                   "Расположение органов"
                 , "Листки брюшины"
                 , "Сращения"
                 , "Наличие свободной жидкости в брюшной полости"
                 , "Внешний вид и размеры внутренних органов до вскрытия:"
                 ]
               , [ -- 21. Грудная полость
                   "Расположение органов"
                 , "Объем легкого"
                 , "Наружный осмотр сердца:"
                 , "Вилочковая железа:"
                 ]
               , [ -- 22. Полость черепа
                   "Мягкие покровы головы при отделении их от черепа"
                 , "Кости черепа"
                 , "Оболочки головного мозга:"
                 , "Головной мозг:"
                 , "Вещество мозга"
                 , "Желудочки"
                 , "Мозжечок"
                 , "Продолговатый мозг"
                 , "Сосудистые сплетения"
                 ]
               , [ -- 23. Органы кровообращения
                   "Сердце:"
                 , "Консистенция сердечной мышцы"
                 , "Кровенаполнение полостей сердца"
                 , "Сгустки крови"
                 , "Проходимость предсердно-желудочковых отверстий"
                 , "Толщина стенки желудочков:"
                 , "Миокард:"
                 , "Аорта"
                 , "Легочные артерии"
                 , "Крупные вены"
                 ]
               , [ -- 24. Органы дыхания
                   "Околоносовые пазухи "
                 , "Гортань"
                 , "Трахея"
                 , "Слизистая оболочка трахеи и бронхов"
                 , "Легкие:"
                 , "Водная проба"
                 , "Воздушность"
                 , "Плотность"
                 , "Ткань легкого на разрезе"
                 , "Патологические образования"
                 , "Состояние поперечных срезов:"
                 , "Прикорневые лимфатические узлы"
                 , "Паратрахеальные лимфатические узлы"
                 ]
               , [ -- 25. Органы пищеварения
                   "Язык"
                 , "Небные миндалины"
                 , "Пищевод"
                 , "Желудок"
                 , "Кишка:"
                 , "Печень:"
                 , "Желчный пузырь:"
                 , "Внепеченочные желчные протоки"
                 , "Поджелудочная железа:"
                 ]
               , [ -- 26. Органы мочеполовой системы
                   "Почки:"
                 , "Мочеточники"
                 , "Мочевой пузырь"
                 , "Предстательная железа"
                 , "Матка"
                 , "Маточные трубы"
                 , "Влагалище"
                 , "Яичники"
                 ]
               , [ -- 27. Органы кроветворения
                   "Селезенка:"
                 , "Лимфатические узлы:"
                 , "Костный мозг"
                 ]
               , [ -- 28. Эндокринные железы
                   "Щитовидная железа:"
                 , "Надпочечники:"
                 , "Гипофиз"
                 , "Паращитовидные железы"
                 ]
               , [ -- 29. Костно-мышечная система
                   "Мышцы"
                 , "Кости"
                 , "Суставы"
                 ]
               ]