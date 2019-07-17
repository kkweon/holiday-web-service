module Country
  ( Country(USA, Korea)
  , parseCountry
  , koreaTimeZone
  , getKoreaDay
  , getUsaDay
  ) where

import Data.Char (toLower)
import Data.List.Extra (trim)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Time as Time

data Country
  = Korea
  | USA
  deriving (Show, Eq)

koreaTimeZone :: Time.TimeZone
koreaTimeZone = Time.hoursToTimeZone 9

koreaNames :: Set String
koreaNames = Set.fromList ["korea", "south korea"]

usaNames :: Set String
usaNames = Set.fromList ["usa", "united states", "u.s", "u.s.a"]

parseCountry :: String -> Maybe Country
parseCountry text =
  let t = map toLower . trim $ text
   in getCountry t
  where
    getCountry countryName
      | countryName `Set.member` koreaNames = Just Korea
      | countryName `Set.member` usaNames = Just USA
      | otherwise = Nothing

-- | Get current date in given tz.
getTzDay :: Time.TimeZone -> IO Time.Day
getTzDay tz = do
  Time.LocalTime day _ <- Time.utcToLocalTime tz <$> Time.getCurrentTime
  return day

-- | Get date in Korea.
getKoreaDay :: IO Time.Day
getKoreaDay = getTzDay koreaTimeZone

-- | Get date in the U.S.
getUsaDay :: IO Time.Day
getUsaDay = getTzDay (read "PST")
