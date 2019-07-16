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

getKoreaDay :: IO Time.Day
getKoreaDay = do
  Time.LocalTime koreaDay _ <-
    Time.utcToLocalTime koreaTimeZone <$> Time.getCurrentTime
  return koreaDay

getUsaDay :: IO Time.Day
getUsaDay = do
  let tz = read "PST"
  Time.LocalTime usaDay _ <- Time.utcToLocalTime tz <$> Time.getCurrentTime
  return usaDay
