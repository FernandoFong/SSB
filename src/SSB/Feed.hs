{-# LANGUAGE DeriveGeneric #-}

module SSB.Feed where

import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.ByteArray  as BA
import qualified Data.Text as T
import qualified Data.Aeson as A

import qualified SSB.Identity
import qualified SSB.Message

data Feed = Feed { author :: SSB.Identity.Identity
                 , messages :: [SSB.Message.Message A.Object]
                 , last_message :: Maybe BS.ByteString
                 } deriving (Generic, Eq, Show)

--
-- | JSON Conversions.
--
instance A.ToJSON Feed where
    toJSON f = A.object [T.pack "author"       A..= (A.toJSON . author       $ f),
                         T.pack "messages"     A..= (A.toJSON . messages     $ f),
                         T.pack "last_message" A..= (A.toJSON . last_message $ f)]

instance A.FromJSON Feed
    -- No need to provide a parseJSON implementation.

--
-- | Module specific functions
--
newFeed :: SSB.Identity.Identity -> Feed
newFeed a = Feed {author = a, messages = [], last_message = Nothing}

appendTo :: A.ToJSON a => SSB.Message.Message a -> Feed -> Maybe Feed
appendTo m f = do
  -- Add feed info (sequence and previous)
  -- And sign the new message
  m'  <- Just m { SSB.Message.sequence = toInteger. length . messages $ f
                , SSB.Message.previous = last_message f} >>= SSB.Message.signMessage
  -- Convert from (Message a) to (Message A.Object) for Homogeneous Storage
  m'' <- A.decodeStrict . SSB.Message.encode $ m'
  -- If nothing failed, return new feed
  return $ f { messages     = messages f ++ [m'']
             , last_message = Just . BA.convert . SSB.Message.sha256 . SSB.Message.encode $ m''}
