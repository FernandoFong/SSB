 {-# LANGUAGE DeriveGeneric #-}

module SSB.Message where

import GHC.Generics

import Data.Semigroup ((<>))
import Data.Maybe (isJust, fromJust)
import Data.Either (fromRight)
import Data.Char

import Data.Time.Clock.System

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BS (toStrict)
import qualified Data.ByteString.UTF8   as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteArray         as BA
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A

import qualified Crypto.Error          as C
import qualified Crypto.Hash           as C
import qualified Crypto.PubKey.Ed25519 as C

import qualified Test.QuickCheck as Q

import SSB.Misc
import SSB.Identity

--
-- | Message. As documented on the protocol guide: https://ssbc.github.io/scuttlebutt-protocol-guide/#feeds
--
data Message a = Message {
    previous  :: Maybe BS.ByteString  -- Message ID of the latest message posted in the feed. If this is the very first message then use null. See below for how to compute a message’s ID.
  , author    :: Identity             -- Public key of the feed that the message will be posted in.
  , sequence  :: Integer              -- 1 for the first message in a feed, 2 for the second and so on.
  , timestamp :: Integer              -- Time the message was created. Number of milliseconds since 1 January 1970 00:00 UTC.
  , hash      :: BS.ByteString        -- The fixed string sha256, which is the hash_message function used to compute the message ID.
  , content   :: a                    -- Free-form JSON. It is up to applications to interpret what it means. It’s polite to specify a type field so that applications can easily filter out message types they don’t understand.
  , signature  :: Maybe BS.ByteString -- Maybe signed
  } deriving (Generic, Eq, Show)

--
-- | Arbitrary Message a. For use with QuickCheck
--
-- >>> msg <- generate arbitrary :: IO (Message BS.ByteString)
-- ...
instance (A.ToJSON a, Q.Arbitrary a) => Q.Arbitrary (Message a) where
  arbitrary = do
    a   <- Q.arbitrary
    ts  <- Q.arbitrary
    c   <- Q.arbitrary
    let m = Message { previous             = Nothing
                    , author               = a
                    , SSB.Message.sequence = 0
                    , timestamp            = ts
                    , hash                 = BS.fromString "sha256"
                    , content              = c
                    , signature             = Nothing
                    }
    return . fromJust $ signMessage m

--
-- | JSON Conversions.
--
instance (A.ToJSON a) => A.ToJSON (Message a) where
    toJSON m = A.object $
      [T.pack "previous"  A..= (A.toJSON . previous              $ m),
       T.pack "author"    A..= (A.toJSON . author                $ m),
       T.pack "sequence"  A..= (A.toJSON . SSB.Message.sequence  $ m),
       T.pack "timestamp" A..= (A.toJSON . timestamp             $ m),
       T.pack "hash"      A..= T.pack "sha256",
       T.pack "content"   A..= (A.toJSON . content               $ m)] ++
      [T.pack "signature" A..= A.toJSON sig | isJust sig]
      where
        sig = signature m

instance (A.FromJSON a) => A.FromJSON (Message a)
    -- No need to provide a parseJSON implementation.

--
-- | Message Signature and Verification.
--
signMessage :: A.ToJSON a => Message a -> Maybe (Message a)
signMessage m = do
  seckey <- sk . author $ m
  let sig = B64.encode . BA.convert $
            C.sign seckey pubkey (encode m')
  return $ m {signature = Just sig}
  where
    pubkey = pk . author $ m
    m' = m {signature = Nothing}

verifyMessage :: A.ToJSON a => Message a -> Maybe Bool
verifyMessage m = do
  msgSig <- signature m
  let decSig = fromRight (BS.fromString "") . B64.decode $ msgSig
  sig <- C.maybeCryptoError . C.signature $ decSig
  return $ C.verify pubkey (encode m') sig
  where
    pubkey = pk . author $ m
    m' = m {SSB.Message.signature = Nothing}



-- Generic Configurations

encode :: A.ToJSON a => a -> BS.ByteString
encode = BS.toStrict . A.encodePretty' confPPSSB

confPPSSB = A.Config { A.confIndent = A.Spaces 2
                     , A.confCompare = messageOrder
                     , A.confNumFormat = A.Generic
                     , A.confTrailingNewline = False
                     }
  where messageOrder = (A.keyOrder . fmap T.pack $ -- Message Order
                        ["previous", "author", "sequence", "timestamp", "hash", "content", "type"]) <>
                       (A.keyOrder . fmap T.pack $ -- Contact Order
                        ["contact", "following", "blocking", "pub", "name"]) <>
                       (A.keyOrder . fmap T.pack $ -- Mention Order
                        ["link", "name"]) <>
                       (A.keyOrder . fmap T.pack $ -- Post Order
                        ["root", "branch", "reply", "channel", "rcps", "text", "mentions"])

sha256 :: BS.ByteString -> C.Digest C.SHA256
sha256 = C.hash
