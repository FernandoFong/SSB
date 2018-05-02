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

data Identity = Identity (Maybe SecretKey) PublicKey deriving (Generic, Show)

instance Eq Identity where
  (Identity _ pk) == (Identity _ pk') = pk == pk'

instance Arbitrary Identity where
  arbitrary = do
    sk <- generateSecretKey
    return $ Identity (Just sk) (toPublic sk)

instance ToJSON Identity where
  toJSON identity = String $ T.pack $ prettyPrint identity

instance FromJSON Identity where
  parseJSON = withText "Identity" $ \i -> case parseIdentity $ T.unpack i of
                                            Just identity -> return identity
                                            Nothing -> fail "Not a valid identity"

-- | prettyPrint Identity. @{base64(PublicKey)}.ed25519
-- | Example: @+DOl55sJo1wBqWtRuI3otubyE4M24q34X4wikX+7BXg=.ed25519
--
-- >>> sk <- generateSecretKey
-- >>> prettyPrint (Identity (Just sk) (toPublic sk))
-- ...
prettyPrint :: Identity -> String
prettyPrint (Identity _ pk) = "@" ++ encodedPubKey ++ ".ed25519"
  where encodedPubKey = BS.UTF8.toString (B64.encode $ BS.pack $ BA.unpack pk)


parseIdentity :: String -> Maybe Identity
parseIdentity str = if "@" `L.isPrefixOf` str && ".ed25519" `L.isSuffixOf` str then
                      let encPKStr = L.take (L.length str - 9) . L.drop 1 $ str in
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