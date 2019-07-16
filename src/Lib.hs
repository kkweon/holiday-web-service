{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( app
  ) where

import qualified Country
import qualified Holiday.Service as Service
import qualified Network.Linklater as Linklater
import qualified Network.Wai as Wai

import Data.Text (Text)
import qualified Data.Text as T

app :: Wai.Application
app = Linklater.slashSimple f
  where
    f :: Linklater.Command -> IO Text
    f (Linklater.Command "holiday" _ _ (Just text)) = do
      country <- liftMaybe $ Country.parseCountry (T.unpack text)
      if country == Country.Korea
          then do
            h <- getKoreaHoliday
            koreaDay <- Country.getKoreanDay
            return $ Service.formatHoliday koreaDay h
          else do
            h <- getUsaHoliday
            usaDay <- Country.getUsaDay
            return $ Service.formatHoliday usaDay h
    f _ = return ""

liftMaybe :: Maybe a -> IO a
liftMaybe (Just a) = return a
liftMaybe Nothing = fail "No value"


getUsaHoliday :: IO Service.Holiday
getUsaHoliday = do
    usaDay <- Country.getUsaDay
    case Service.getNearestHoliday usaDay Country.USA of
        Just h -> return h
        Nothing -> fail "Unable to find any holiday in the U.S"


getKoreaHoliday :: IO Service.Holiday
getKoreaHoliday = do
    koreaDay <- Country.getKoreanDay
    case Service.getNearestHoliday koreaDay Country.Korea of
        Just h -> return h
        Nothing -> fail "Unable to find any holiday in Korea"
