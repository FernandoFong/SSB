module SSB.MessageSpec where

import Test.Hspec
import Test.QuickCheck

import Data.ByteString
import Data.Aeson

import SSB.Message

main :: IO ()
main = hspec spec

spec = do
  describe "Decode should be an identity on messages" $ do
    it "ByteString" $ property $
      \msg -> (decode . encode) msg == Just (msg :: Message ByteString)
