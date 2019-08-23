{-# LANGUAGE OverloadedStrings #-}
module Labels where
import           Data.List (findIndex, isSubsequenceOf)
import           Data.Text (Text, unpack)

labels0 :: [Text]
labels0 = [
            "Название мед. организации"
          , "Адрес мед. организации"
          -- , "Код формы по ОКУД"
          -- , "Код учреждения по ОКПО"
          ]

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
          , "Проведено койко-дней (????)"
          , "Место жительства"
          , "Местность"
          , "Семейное положение"
          , "Образование"
          , "Занятость"
          , "Дата поступления"
          , "ПОМЕНЯТЬ НАЗВАНИЕ"
          , "ФИО лечащего врача"
          , "Леч. врач (зав. отделением)\nприсутствовал на вскрытии?"
          , "Дата вскрытия"
          , "Основные клинические данные"
          , "код по МКБ-Х"
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

labelsInner2 :: [[Text]]
labelsInner2 = [
                 [ -- 19. Наружный осмотр тела
                   "Пол трупа"
                 , "Длина тела (см)"
                 , "Масса тела (кг)"
                 , "Телосложение"
                 , "Cостояние питания"
                 , "Cостояние мышечной и костной систем"
                 -- , "Кожный покров: головы"
                 -- , "Кожный покров: шеи"
                 -- , "Кожный покров: груди"
                 -- , "Кожный покров: туловища"
                 -- , "Кожный покров: конечностей"
                 -- , "Трупные пятна и их расположение"
                 -- , "Выраженность и распространенность трупного окоченения"
                 -- , "Состояние естественных отверстий"
                 -- , "Наружные половые органы сформированы (?)"
                 -- , "Операционные раны: длина разреза (см)"
                 -- , "Операционные раны: характер операционной раны"
                 -- , "Операционные раны: швы"
                 -- , "Операционные раны: выделения из раны"
                 -- , "Следы инъекций и изменения в их зоне"
                 ]
               , [ -- 20. Брюшная полость
                   "Расположение органов"
                 , "Листки брюшины"
                 , "Сращения"
                 , "Наличие свободной жидкости в брюшной полости"
                 , "Внешний вид и размеры внутренних органов до вскрытия: диафрагма"
                 , "Внешний вид и размеры внутренних органов до вскрытия: печень"
                 , "Внешний вид и размеры внутренних органов до вскрытия: селезенка"
                 , "Внешний вид и размеры внутренних органов до вскрытия: большой сальник"
                 , "Внешний вид и размеры внутренних органов до вскрытия: кишечник"
                 , "Внешний вид и размеры внутренних органов до вскрытия: мочевой пузырь"
                 , "Внешний вид и размеры внутренних органов до вскрытия: червеобразный отросток"
                 ]
               , [ -- 21. Грудная полость
                   "Расположение органов"
                 , "Объем легкого"
                 , "Наружный осмотр сердца: форма"
                 , "Наружный осмотр сердца: расположение"
                 , "Вилочковая железа: расположение"
                 , "Вилочковая железа: величина"
                 ]
               , [ -- 22. Полость черепа
                   "Мягкие покровы головы при отделении их от черепа"
                 , "Кости черепа"
                 , "Оболочки головного мозга: мягкая"
                 , "Оболочки головного мозга: паутинная"
                 , "Оболочки головного мозга: твердая"
                 , "Оболочки головного мозга: кровенаполнение сосудов (?)"
                 , "Головной мозг: объем"
                 , "Головной мозг: масса"
                 , "Головной мозг: консистенция"
                 , "Вещество мозга"
                 , "Желудочки"
                 , "Мозжечок"
                 , "Продолговатый мозг"
                 , "Осудистые сплетения"
                 ]
               , [ -- 23. Органы кровообращения
                   "Сердце: размеры"
                 , "Сердце: масса"
                 , "Сердце: эпикард"
                 , "Сердце: перикард"
                 , "Консистенция сердечной мышцы"
                 , "Кровенаполнение полостей сердца"
                 , "Сгустки крови"
                 , "Проходимость предсердно-желудочковых отверстий"
                 , "Толщина стенки желудочков: правого"
                 , "Толщина стенки желудочков: левого"
                 , "Толщина стенки желудочков: межжелудочковой перегородки (?)"
                 , "Миокард: эндокард"
                 , "Миокард: клапаны сердца"
                 , "Миокард: венечные артерии"
                 , "Аорта"
                 , "Легочные артерии"
                 , "Крупные вены"
                 ]
               , [ -- 24. Органы дыхания
                   "Околоносовые пазухи "
                 , "Гортань"
                 , "Трахея"
                 , "Слизистая оболочка трахеи и бронхов"
                 , "Легкие: размеры правого"
                 , "Легкие: размеры левого"
                 , "Легкие: масса"
                 , "Легкие: форма"
                 , "Легкие: плевральные листки"
                 , "Водная проба"
                 , "Воздушность"
                 , "Плотность"
                 , "Ткань легкого на разрезе"
                 , "Патологические образования"
                 , "Состояние поперечных срезов: сосудов"
                 , "Состояние поперечных срезов: бронхов"
                 , "Прикорневые лимфатические узлы"
                 , "Паратрахеальные лимфатические узлы"
                 ]
               , [ -- 25. Органы пищеварения
                   "Околоносовые пазухи "
                 , "Гортань"
                 , "Трахея"
                 , "Слизистая оболочка трахеи и бронхов"
                 , "Легкие: размеры правого"
                 , "Легкие: размеры левого"
                 , "Легкие: масса"
                 , "Легкие: форма"
                 , "Легкие: плевральные листки"
                 , "Водная проба"
                 , "Воздушность"
                 , "Плотность"
                 , "Ткань легкого на разрезе"
                 , "Патологические образования"
                 , "Состояние поперечных срезов: сосудов"
                 , "Состояние поперечных срезов: бронхов"
                 , "Прикорневые лимфатические узлы"
                 , "Паратрахеальные лимфатические узлы"
                 ]
               , [ -- 26. Органы мочеполовой системы
                   "Почки: размеры"
                 , "Почки: масса"
                 , "Почки: консистенция"
                 , "Почки: характер поверхности"
                 , "Почки: вид на разрезе"
                 , "Почки: толщина коркового вещества"
                 , "Почки: слизистая оболочка лоханок"
                 , "Мочеточники"
                 , "Мочевой пузырь"
                 , "Предстательная железа"
                 , "Матка"
                 , "Маточные трубы"
                 , "Влагалище"
                 , "Яичники"
                 ]
               , [ -- 27. Органы кроветворения
                   "Селезенка: размеры"
                 , "Селезенка: масса"
                 , "Селезенка: консистенция"
                 , "Селезенка: вид снаружи"
                 , "Селезенка: вид на разрезе"
                 , "Лимфатические узлы: брыжейки"
                 , "Лимфатические узлы: средостения"
                 , "Лимфатические узлы: шеи"
                 , "Костный мозг"
                 ]
               , [ -- 28. Эндокринные железы
                   "Щитовидная железа: размеры"
                 , "Щитовидная железа: консистенция"
                 , "Щитовидная железа: вид снаружи"
                 , "Щитовидная железа: вид на разрезе"
                 , "Надпочечники: форма"
                 , "Надпочечники: цвет на разрезе"
                 , "Надпочечники: рисунок ткани"
                 , "Гипофиз"
                 , "Паращитовидные железы"
                 ]
               , [ -- 29. Костно-мышечная система
                   "Мышцы"
                 , "Кости"
                 , "Суставы"
                 ]
               ]
