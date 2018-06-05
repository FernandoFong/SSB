{-# LANGUAGE DeriveGeneric #-}

module SSB.Identity where

import GHC.Generics

import Control.Monad

import Data.List              as L
import Data.ByteString        as BS
import Data.ByteString.UTF8   as BS.UTF8
import Data.ByteString.Base64 as B64
import Data.ByteArray         as BA
import Data.Text              as T

import Data.Aeson             as Aeson

import Crypto.Error
import Crypto.PubKey.Ed25519
import Crypto.Random.Types

import Test.QuickCheck

data Identity = Identity {
    sk :: Maybe SecretKey
  , pk :: PublicKey
  } deriving (Generic, Show)

instance Eq Identity where
  id1 == id2 = pk id1 == pk id2

instance Arbitrary Identity where
  arbitrary = do
    skey <- generateSecretKey
    return Identity {sk = Just skey, pk = toPublic skey}

instance ToJSON Identity where
  toJSON identity = String $ T.pack $ prettyPrint identity

instance FromJSON Identity where
  parseJSON = withText "Identity" $ \i -> case parseIdentity $ T.unpack i of
                                            Just identity -> return identity
                                            Nothing -> fail "Not a valid identity"

-- | prettyPrint Identity. @{base64(PublicKey)}.ed25519
-- | Example: @+DOl55sJo1wBqWtRuI3otubyE4M24q34X4wikX+7BXg=.ed25519
prettyPrint :: Identity -> String
prettyPrint (Identity _ pk) = "@" ++ encodedPubKey ++ ".ed25519"
  where encodedPubKey = BS.UTF8.toString (B64.encode $ BS.pack $ BA.unpack pk)

-- | parseIdentity Identity. @{base64(PublicKey)}.ed25519
--
-- Will only get public key for an identity.
-- Example: @+DOl55sJo1wBqWtRuI3otubyE4M24q34X4wikX+7BXg=.ed25519
parseIdentity :: String -> Maybe Identity
parseIdentity str = if "@" `L.isPrefixOf` str && ".ed25519" `L.isSuffixOf` str then
                      let encPKStr = L.drop 1 str in
                      let encPK    = B64.decode $ BS.UTF8.fromString encPKStr in
                      case encPK of
                        Right encodedPubKey' -> case publicKey encodedPubKey' of
                                                  CryptoPassed pk' -> Just $ Identity Nothing pk'
                                                  CryptoFailed _   -> Nothing
                        Left _ -> Nothing
                    else
                      Nothing

--
-- TODO: Move to own module
--
instance MonadRandom Gen where
  getRandomBytes n = BA.pack <$> replicateM n arbitrary
