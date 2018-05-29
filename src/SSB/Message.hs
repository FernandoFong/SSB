{-# LANGUAGE DeriveGeneric #-}

module SSB.Message where

import GHC.Generics

import Data.Semigroup ((<>))
import Data.Maybe (isJust, fromJust)
import Data.Either (fromRight)

import Data.Char
import Data.ByteString        as BS
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8   as BSUTF8
import Data.ByteString.Base64 as B64
import Data.ByteArray         as BA

import Data.Text              as T
import Data.Text.Encoding     as T.Enc

import Data.Time.Clock.System

import Data.Aeson
import Data.Aeson.Encode.Pretty

import Crypto.Error
import Crypto.PubKey.Ed25519 as Crypto

import Test.QuickCheck

import SSB.Misc
import SSB.Identity

--
-- | Message. As documented on the protocol guide: https://ssbc.github.io/scuttlebutt-protocol-guide/#feeds
--
data Message a = Message {
    previous  :: Maybe ByteString -- Message ID of the latest message posted in the feed. If this is the very first message then use null. See below for how to compute a message’s ID.
  , author    :: Identity   -- Public key of the feed that the message will be posted in.
  , sequence  :: Integer    -- 1 for the first message in a feed, 2 for the second and so on.
  , timestamp :: DotNetTime -- Time the message was created. Number of milliseconds since 1 January 1970 00:00 UTC.
  , hash      :: ByteString -- The fixed string sha256, which is the hash function used to compute the message ID.
  , content   :: a          -- Free-form JSON. It is up to applications to interpret what it means. It’s polite to specify a type field so that applications can easily filter out message types they don’t understand.
  , signature :: Maybe ByteString
  } deriving (Generic, Eq, Show)

--
-- | Arbitrary Message a. For use with QuickCheck
--
instance (ToJSON a, Arbitrary a) => Arbitrary (Message a) where
  arbitrary = do
    a   <- arbitrary
    ts  <- arbitrary
    c   <- arbitrary
    let m = Message {previous = Nothing, author = a, SSB.Message.sequence = 0,
                     timestamp = ts, hash = fromString "sha256", content = c, SSB.Message.signature = Nothing}
    return . fromJust $ signMessage m

--
-- | JSON Conversions.
--
instance (ToJSON a) => ToJSON (Message a) where
    toEncoding m | isJust sig = pairs (defaultMessage <> T.pack "signature" .= toJSON sig)
                 | otherwise  = pairs defaultMessage
                   where
                     sig = SSB.Message.signature m
                     defaultMessage = T.pack "previous"  .= (toJSON . previous              $ m) <>
                                      T.pack "author"    .= (toJSON . author                $ m) <>
                                      T.pack "sequence"  .= (toJSON . SSB.Message.sequence  $ m) <>
                                      T.pack "timestamp" .= (toJSON . timestamp             $ m) <>
                                      T.pack "hash"      .= (toJSON . hash                  $ m) <>
                                      T.pack "content"   .= (toJSON . content               $ m)

instance (FromJSON a) => FromJSON (Message a)
    -- No need to provide a parseJSON implementation.

--
-- | Message Signature and Verification.
--
signMessage :: ToJSON a => Message a -> Maybe (Message a)
signMessage m = do
  seckey <- sk . author $ m
  let sig = B64.encode . convert $
            sign seckey pubkey (SSB.Message.encode m') :: ByteString
  return $ m {SSB.Message.signature = Just sig}
  where
    pubkey = pk . author $ m
    m' = m {SSB.Message.signature = Nothing}

verifyMessage :: ToJSON a => Message a -> Maybe Bool
verifyMessage m = do
  msgSig <- SSB.Message.signature m
  let decSig = fromRight (fromString "") . B64.decode $ msgSig
  sig <- maybeCryptoError . Crypto.signature $ decSig
  return $ verify pubkey (SSB.Message.encode m') sig
  where
    pubkey = pk . author $ m
    m' = m {SSB.Message.signature = Nothing}



-- Generic Configurations

encode :: ToJSON a => a -> ByteString
encode = toStrict . encodePretty' confPPSSB

confPPSSB = Config { confIndent = Spaces 2,
                     confCompare = mempty,
                     confNumFormat = Generic,
                     confTrailingNewline = False }
