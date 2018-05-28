module SSB.MessageSpec where

import Test.Hspec
import Test.QuickCheck

import Data.ByteString
import Data.Aeson

import SSB.Message
import SSB.Message.Post

main :: IO ()
main = hspec spec

spec = do
  describe "Decode should be a left inverse of encode identity on messages" $ do
    it "ByteString" $ property $
      \msg -> (decodeStrict . SSB.Message.encode) msg == Just (msg :: Message ByteString)
    it "Post" $ property $
      \msg -> (decodeStrict . SSB.Message.encode) msg == Just (msg :: Message Post)
