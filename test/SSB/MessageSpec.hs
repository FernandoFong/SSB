module SSB.MessageSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Aeson           as A

import SSB.Identity
import SSB.Message
import SSB.Message.Contact
import SSB.Message.Post

main :: IO ()
main = hspec spec

spec = do
  describe "Decode should be a left inverse of encode identity on messages" $ do
    it "ByteString" $ property $
      \msg -> (A.decodeStrict . encode) msg == Just (msg :: Message BS.ByteString)
    it "Post" $ property $
      \msg -> (A.decodeStrict . encode) msg == Just (msg :: Message Post)
  describe "Signing a message should be verifiable" $ do
    it "ByteString" $ property $
      \msg -> (signMessage (msg :: Message BS.ByteString) >>= verifyMessage) == Just True
    it "Post" $ property $
      \msg -> (signMessage (msg :: Message Post) >>= verifyMessage) == Just True
  describe "Signing again should yield the same signature" $ do
    it "ByteString" $ property $
      \msg -> (signMessage msg >>= signMessage) == signMessage (msg :: Message BS.ByteString)
    it "Post" $ property $
      \msg -> (signMessage msg >>= signMessage) == signMessage (msg :: Message Post)

  describe "Attempt to parse message contact examples" $ do
    context "Example 1" $ do
      it "Decodes" $ example $ do
        raw_msg <- BS.readFile "test/realMessageContact1"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Contact)
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        raw_msg <- BS.readFile "test/realMessageContact1"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Contact)
        pending -- (msg >>= verifyMessage) `shouldBe` Just True
    context "Example 2" $ do
      it "Decodes" $ example $ do
        raw_msg <- BS.readFile "test/realMessageContact2"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Contact)
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        raw_msg <- BS.readFile "test/realMessageContact2"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Contact)
        (msg >>= verifyMessage) `shouldBe` Just True

  describe "Attempt to parse message post examples" $ do
    context "Example 1" $ do
      it "Decodes" $ example $ do
        raw_msg <- BS.readFile "test/realMessagePost1"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Post)
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        raw_msg <- BS.readFile "test/realMessagePost1"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Post)
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example 2" $ do
      it "Decodes" $ example $ do
        raw_msg <- BS.readFile "test/realMessagePost2"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Post)
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        raw_msg <- BS.readFile "test/realMessagePost2"
        let msg = A.decodeStrict raw_msg :: Maybe (Message Post)
        (msg >>= verifyMessage) `shouldBe` Just True
