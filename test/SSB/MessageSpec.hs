{-# LANGUAGE ScopedTypeVariables #-}

module SSB.MessageSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Aeson           as A

import SSB.Misc
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
        msg :: Maybe (Message Contact) <- loadMessage "test/samples/real-messages/contact1"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message Contact) <- loadMessage "test/samples/real-messages/contact1"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example 2" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message Contact) <- loadMessage "test/samples/real-messages/contact2"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message Contact) <- loadMessage "test/samples/real-messages/contact2"
        (msg >>= verifyMessage) `shouldBe` Just True

  describe "Attempt to parse message post examples" $ do
    context "Example 1" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message Post) <- loadMessage "test/samples/real-messages/post1"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message Post) <- loadMessage "test/samples/real-messages/post1"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example 2" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message Post) <- loadMessage "test/samples/real-messages/post2"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message Post) <- loadMessage "test/samples/real-messages/post2"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example 3" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message Post) <- loadMessage "test/samples/real-messages/post3"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message Post) <- loadMessage "test/samples/real-messages/post3"
        pending -- (msg >>= verifyMessage) `shouldBe` Just True

  describe "Attempt to parse and validate valid SSBC examples" $ do
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid1"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid1"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid2"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid2"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message BS.ByteString) <- loadMessage "test/samples/ssb-validate/valid3"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message BS.ByteString) <- loadMessage "test/samples/ssb-validate/valid3"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message BS.ByteString) <- loadMessage "test/samples/ssb-validate/valid4"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message BS.ByteString) <- loadMessage "test/samples/ssb-validate/valid4"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid5"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid5"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid6"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid6"
        (msg >>= verifyMessage) `shouldBe` Just True
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid7"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid7"
        pending -- (msg >>= verifyMessage) `shouldBe` Just True
    context "Example" $ do
      it "Decodes" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid8"
        isJust msg `shouldBe` True
      it "Verifies" $ example $ do
        msg :: Maybe (Message A.Object) <- loadMessage "test/samples/ssb-validate/valid8"
        pending -- (msg >>= verifyMessage) `shouldBe` Just True
