{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( app
  ) where

import qualified Country
import qualified Data.Time as Time
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
      let country = Country.parseCountry (T.unpack text)
      case country of
        Just Country.Korea -> buildHoliday getKoreaHoliday Country.getKoreaDay
        Just Country.USA -> buildHoliday getUsaHoliday Country.getUsaDay
        _ -> return "Only USA and KOREA are supported at the moment."
      where
        buildHoliday :: IO Service.Holiday -> IO Time.Day -> IO Text
        buildHoliday ioHoliday ioDay = do
          h <- ioHoliday
          d <- ioDay
          return . Service.formatHoliday d $ h
    f _ = return ""

getHoliday :: IO Time.Day -> Country.Country -> IO Service.Holiday
getHoliday ioDay country = do
  day <- ioDay
  case Service.getNearestHoliday day country of
    Just h -> return h
    Nothing -> fail $ "No holiday found in " ++ show country

getUsaHoliday :: IO Service.Holiday
getUsaHoliday = getHoliday Country.getUsaDay Country.USA

getKoreaHoliday :: IO Service.Holiday
getKoreaHoliday = getHoliday Country.getKoreaDay Country.Korea
