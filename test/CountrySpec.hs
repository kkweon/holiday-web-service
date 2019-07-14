module CountrySpec where

import Country
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "CountrySpec" $ do
    it "returns Korea when korea" $ parseCountry "korea" `shouldBe` Just Korea
    it "returns USA when usa" $ parseCountry "usa" `shouldBe` Just USA
    it "returns correctly with special chars and upper cases" $ do
      parseCountry "United States" `shouldBe` Just USA
      parseCountry "U.S.A" `shouldBe` Just USA
    it "returns Nothing otherwise" $
      parseCountry "No Country" `shouldBe` Nothing
