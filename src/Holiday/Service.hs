module Holiday.Service where

import qualified Data.Holiday.Korea as HK
import qualified Data.Holiday.Model as HK
import qualified Data.Holiday.USA as USH
import qualified Data.Text as T
import qualified Data.Time as Time

import Control.Monad (msum)
import Country (Country(Korea, USA))
import Data.Text (Text)

data Holiday =
  Holiday
    { holiday_localDay :: Time.Day
    , holiday_name :: String
    }
  deriving (Eq, Show)

getNearestHoliday :: Time.Day -> Country -> Maybe Holiday
getNearestHoliday day USA = msum . map go $ [0 .. 365]
  where
    go :: Integer -> Maybe Holiday
    go x = do
      let newDay = Time.addDays x day
      USH.Holiday _holiday _name <- USH.getHoliday newDay
      return $ Holiday _holiday _name
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

-- | Labor Day (2019-09-02, xxx days left)
formatHoliday :: Time.Day -> Holiday -> Text
formatHoliday beg (Holiday day name) =
  let diff = Time.diffDays day beg -- day - beg
   in T.pack $
      unwords [name, "(" ++ show day ++ ",", show diff ++ " days left" ++ ")"]
