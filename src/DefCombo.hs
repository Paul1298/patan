{-# LANGUAGE OverloadedStrings #-}
module DefCombo where
import           Data.Text (Text)

import           Xman

initDef1 :: IO [[Text]]
initDef1 = do
  fios <- fioX
  numbers <- numberX
  departments <- deptX
  let
    orgs  = [ "Областное бюджетное учреждение здравоохранения «Курская городская клиническая больница скорой медицинской помощи»" ]
    areas = [ "Городская", "Сельская" ]
    marriages = [
                  "Состоял в зарегистрированном браке"
                , "Не состоял"
                , "Неизвестно"
                ]
    educations = [
                   "Профессиональное: высшее"
                 , "Профессиональное: неполное высшее"
                 , "Профессиональное: среднее"
                 , "Профессиональное: начальное"
                 , "Общее: среднее (полное)"
                 , "Общее: основное"
                 , "Общее: начальное"
                 , "Не имеет начального образования"
                 , "Неизвестно"
                 ]
    employments = [
                    "Руководители и специалисты высшего уровня квалификации"
                  , "Прочие специалисты"
                  , "Квалифицированные рабочие"
                  , "Неквалифицированные рабочие"
                  , "Занятые на военной службе"
                  , "Пенсионеры"
                  , "Студенты и учащиеся"
                  , "Работавшие в личном подсобном хозяйстве"
                  , "Безработные"
                  , "Прочие"
                  ]
  return [
           []
         , []
         , numbers
         , fios
         , orgs
         , departments
         , []
         , []
         , []
         , []
         , []
         , []
         , areas
         , marriages
         , educations
         , employments
         , []
         , []
         , []
         , []
         , []
         , []
         , []
         , []
         , []
         , []
         ]

defInner2 :: IO [[[Text]]]
defInner2 = return [
                     [ -- 19. Наружный осмотр тела
                       []
                     , []
                     , ["правильное", "астеническое", "гиперстеническое"]
                     , ["удовлетворительное", "повышенное", "пониженное"]
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 20. Брюшная полость
                       ["правильное", "?"]
                     , ["гладкие", "блестящие", "увлажнены"]
                     , ["отсутствуют", "имеются"]
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 21. Грудная полость
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 22. Полость черепа
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 23. Органы кровообращения
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 24. Органы дыхания
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 25. Органы пищеварения
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 26. Органы мочеполовой системы
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 27. Органы кроветворения
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 28. Эндокринные железы
                       []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     , []
                     ]
                   , [ -- 29. Костно-мышечная система
                       []
                     , []
                     , []
                     ]
                   , []
                   , []
                   , []
                   , []
                   ]
