module UnluckyDays where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

unluckyDays :: Integer -> Int
unluckyDays year =
  foldl
    (\acc month ->
       let date = fromGregorian year month 13
       in case (toWeekDate date) of
            (_, _, 5) -> acc + 1
            _ -> acc)
    0
    [1 .. 12]
