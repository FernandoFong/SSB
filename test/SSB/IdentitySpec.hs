module SSB.IdentitySpec where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec = do
  describe "Empty Boilerplate" $ do
    it "1 = 1" $ do
      1 `shouldBe` 1
