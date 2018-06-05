{-# LANGUAGE DeriveGeneric #-}

module SSB.Message.Post where

import GHC.Generics

import Data.Semigroup
import Data.ByteString
import Data.Text as T

import Data.Aeson
import Data.Aeson.Types

import Test.QuickCheck

import SSB.Identity

import SSB.Misc

data Mention = Mention {
    link :: ByteString
  , name :: ByteString
  } deriving (Generic, Eq, Show)

instance ToJSON Mention where
  toEncoding m = pairs (T.pack "link" .= (toJSON . link $ m) <>
                        T.pack "name" .= (toJSON . name $ m))

instance FromJSON Mention where
    -- No need to provide a parseJSON implementation.

data Post = Post {
      text     :: Text
    , root     :: Maybe ByteString
    , branch   :: Maybe ByteString
    , reply    :: Maybe Object
    , channel  :: Maybe ByteString
    , mentions :: Maybe [Mention]
    , recps    :: Maybe [Identity]
    } deriving (Generic, Eq, Show)

instance Arbitrary Post where
  arbitrary = do
    a_text <- arbitrary
    return Post { text     = T.pack a_text
                , root     = Nothing
                , branch   = Nothing
                , reply    = Nothing
                , channel  = Nothing
                , mentions = Nothing
                , recps    = Nothing
                }

instance (ToJSON Post) where
  toEncoding post = pairs (T.pack "type" .= toJSON "post" <>
                           T.pack "post" .= (toJSON . text $ post))

instance (FromJSON Post) where
  -- Generic
  -- TODO: Verify "type": "post"
