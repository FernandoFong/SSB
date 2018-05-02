{-# LANGUAGE DeriveGeneric #-}

module SSB.Message where

import GHC.Generics

import Data.Char
import Data.ByteString
import Data.ByteString.UTF8
import Data.Text.Encoding

import Data.Time.Clock.System

import Data.Aeson

import Test.QuickCheck

import SSB.Identity

-- | Message. As documented on the protocol guide: https://ssbc.github.io/scuttlebutt-protocol-guide/#feeds
data Message a = Message {
    previous  :: Maybe ByteString -- Message ID of the latest message posted in the feed. If this is the very first message then use null. See below for how to compute a message’s ID.
  , author    :: Identity   -- Public key of the feed that the message will be posted in.
  , sequence  :: Integer    -- 1 for the first message in a feed, 2 for the second and so on.
  , timestamp :: DotNetTime -- Time the message was created. Number of milliseconds since 1 January 1970 00:00 UTC.
  , hash      :: ByteString -- The fixed string sha256, which is the hash function used to compute the message ID.
  , content   :: a          -- Free-form JSON. It is up to applications to interpret what it means. It’s polite to specify a type field so that applications can easily filter out message types they don’t understand.
  } deriving (Generic, Eq, Show)

instance Arbitrary a => Arbitrary (Message a) where
  arbitrary = do
    p  <- arbitrary
    a  <- arbitrary
    s  <- arbitrary
    ts <- arbitrary
    h  <- arbitrary
    c  <- arbitrary
    return Message {previous = p, author = a, SSB.Message.sequence = s,
                    timestamp = ts, hash = h, content = c}

instance (ToJSON a) => ToJSON (Message a) where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance (FromJSON a) => FromJSON (Message a)
    -- No need to provide a parseJSON implementation.


--
-- TODO: Move to specific module
--
instance Arbitrary ByteString where
  arbitrary = fromString . Prelude.filter isAlpha . getPrintableString <$> (arbitrary :: Gen PrintableString)

instance Arbitrary DotNetTime where
  arbitrary = do
    time <- arbitrary :: Gen (Positive Integer)
    let sys_time = MkSystemTime {systemSeconds = fromInteger . getPositive $ time, systemNanoseconds = 0} in
      return DotNetTime {fromDotNetTime = systemToUTCTime sys_time}

instance ToJSON ByteString where
  toJSON bs = String $ decodeUtf8 bs

instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ \bs -> pure $ encodeUtf8 bs
