{-# LANGUAGE DeriveGeneric #-}

module SSB.Message.Post where

import GHC.Generics

import Data.Semigroup
import Data.Maybe
import Data.ByteString
import Data.Text as T

import Data.Aeson
import Data.Aeson.Types

import SSB.Misc
import SSB.Identity

import Test.QuickCheck

data Mention = Mention {
    link :: ByteString
  , name :: Maybe ByteString
  } deriving (Generic, Eq, Show)

instance ToJSON Mention where
  toJSON m = object $
    (T.pack "link" .= (toJSON . link $ m)) :
    [T.pack "name" .= (toJSON . name $ m) | isJust . name $ m]

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
  toJSON p = object $
    [T.pack "type"     .= toJSON "post",
     T.pack "text"     .= (toJSON . text     $ p)] ++
    [T.pack "root"     .= (toJSON . root     $ p) | isJust . root     $ p] ++
    [T.pack "branch"   .= (toJSON . branch   $ p) | isJust . branch   $ p] ++
    [T.pack "reply"    .= (toJSON . reply    $ p) | isJust . reply    $ p] ++
    [T.pack "channel"  .= (toJSON . channel  $ p) | isJust . channel  $ p] ++
    [T.pack "mentions" .= (toJSON . mentions $ p) | isJust . mentions $ p] ++
    [T.pack "recps"    .= (toJSON . recps    $ p) | isJust . recps    $ p]

instance (FromJSON Post) where
  -- Generic
  -- TODO: Verify "type": "post"
