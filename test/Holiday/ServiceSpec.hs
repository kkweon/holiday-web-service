module Holiday.ServiceSpec where

import Holiday.Service
import Test.Hspec

import qualified Country
import qualified Data.Time as Time

spec :: Spec
spec =
  describe "Holiday.ServiceSpec" $ do
    it "returns Independece Day when 2019/07/3 in the U.S" $
      let day = Time.fromGregorian 2019 7 4
       in getNearestHoliday day Country.USA `shouldBe`
          Just (Holiday day "Independence Day")
    it "returns a nearest holiday when 2019/07/14 in Korea" $
      let day = Time.fromGregorian 2019 7 14
       in getNearestHoliday day Country.Korea `shouldBe`
          Just
            (Holiday (Time.fromGregorian 2019 8 15) "National Liberation Day")
