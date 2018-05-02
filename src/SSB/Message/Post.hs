{-# LANGUAGE DeriveGeneric #-}

module SSB.Message.Post where

import GHC.Generics

import Data.Semigroup

import Data.Text as T

import Data.Aeson

import Test.QuickCheck

newtype Post = Post {
    text :: Text
    } deriving (Generic, Eq, Show)

instance Arbitrary Post where
  arbitrary = do
    a_text <- arbitrary
    return Post {text = T.pack a_text}

instance (ToJSON Post) where
  toEncoding post = pairs (T.pack "type" .= toJSON "post" <>
                           T.pack "post" .= (toJSON . text $ post))

instance (FromJSON Post) where
  -- Generic
  -- TODO: Verify "type": "post"
