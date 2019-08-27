{-# LANGUAGE OverloadedStrings #-}
module DefCombo where
import           Data.Text (Text)

import           Xman

initDef1 :: IO [Maybe [Text]]
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
           Just []
         , Just []
         , Just numbers
         , Just fios
         , Just orgs
         , Just departments
         , Just []
         , Just []
         , Just []
         , Just []
         , Just []
         , Just []
         , Just areas
         , Just marriages
         , Just educations
         , Just employments
         , Just []
         , Just []
         , Just []
         , Just []
         , Just []
         , Just []
         , Just []
         , Nothing
         , Nothing
         , Nothing
         ]

defInner2 :: IO [[Maybe [Text]]]
defInner2 = return [
                     [ -- 19. Наружный осмотр тела
                       Just []
                     , Just []
                     , Just []
                     , Just ["правильное", "астеническое", "гиперстеническое"]
                     , Just ["удовлетворительное", "повышенное", "пониженное"]
                     , Nothing
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     -- , Just []
                     ]
                   , [ -- 20. Брюшная полость
                       Just ["правильное", "?"]
                     , Just ["гладкие", "блестящие", "увлажнены"]
                     , Just ["отсутствуют", "имеются"]
                     , Nothing
                     , Nothing
                     ]
                   , [ -- 21. Грудная полость
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 22. Полость черепа
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 23. Органы кровообращения
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 24. Органы дыхания
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 25. Органы пищеварения
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 26. Органы мочеполовой системы
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 27. Органы кроветворения
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 28. Эндокринные железы
                       Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     , Just []
                     ]
                   , [ -- 29. Костно-мышечная система
                       Just []
                     , Just []
                     , Just []
                     ]
                   , []
                   , []
                   , []
                   , []
                   ]
