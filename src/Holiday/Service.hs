module Holiday.Service where

import Country (Country(Korea, USA))
import qualified Data.Holiday.Korea as HK
import qualified Data.Holiday.Model as HK
import qualified Data.Time as Time

data Holiday =
  Holiday
    { holiday_localDay :: Time.Day
    , holiday_name :: String
    }
  deriving (Eq, Show)

getNearestHoliday :: Time.Day -> Country -> Maybe Holiday
getNearestHoliday _ USA = undefined
getNearestHoliday day Korea = do
  koreaHoliday <- HK.getNearestHoliday day
  let newDay =
        case HK.date koreaHoliday of
          HK.YMD (y, m, d) -> Time.fromGregorian y m d
          HK.MD (m, d) -> Time.fromGregorian (getYear day) m d
  return $ Holiday newDay (HK.name koreaHoliday)
  where
    getYear _day =
      let (y, _, _) = Time.toGregorian _day
       in y
