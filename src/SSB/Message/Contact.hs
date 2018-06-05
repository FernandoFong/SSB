{-# LANGUAGE DeriveGeneric #-}

module SSB.Message.Contact where

import GHC.Generics

import Data.Semigroup
import Data.Maybe
import Data.ByteString
import Data.Text as T

import Data.Aeson
import Data.Aeson.Types

import Test.QuickCheck

import SSB.Identity

import SSB.Misc

data Contact = Contact {
      contact   :: Identity
    , following :: Maybe Bool
    , blocking  :: Maybe Bool
    , pub       :: Maybe Bool
    , name      :: Maybe ByteString
    } deriving (Generic, Eq, Show)

instance Arbitrary Contact where
  arbitrary = do
    c <- arbitrary
    f <- arbitrary
    b <- arbitrary
    p <- arbitrary
    return Contact { contact   = c
                   , following = f
                   , blocking  = b
                   , pub       = p
                   , name      = Nothing
                   }

instance (ToJSON Contact) where
  toJSON c = object $
             [T.pack "type"      .= T.pack "contact",
              T.pack "contact"   .= (toJSON . contact   $ c)] ++
             [T.pack "following" .= (toJSON . following $ c) | isJust . following $ c] ++
             [T.pack "blocking"  .= (toJSON . blocking  $ c) | isJust . blocking  $ c] ++
             [T.pack "pub"       .= (toJSON . pub       $ c) | isJust . pub       $ c] ++
             [T.pack "name"      .= (toJSON . name      $ c) | isJust . name      $ c]

instance (FromJSON Contact) where
  -- Generic
  -- TODO: Verify "type": "contact"
