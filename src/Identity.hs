module Identity where

import Data.List              as List
import Data.ByteString        as BS
import Data.ByteString.Char8  as BS.C8
import Data.ByteString.Base64 as B64
import Data.ByteArray         as BA
import Crypto.Error
import Crypto.PubKey.Ed25519

data Identity = Identity (Maybe SecretKey) PublicKey deriving (Eq, Show)

-- | prettyPrint Identity. @{base64(PublicKey)}.ed25519
-- | Example: @+DOl55sJo1wBqWtRuI3otubyE4M24q34X4wikX+7BXg=.ed25519
--
-- >>> sk <- generateSecretKey
-- >>> prettyPrint (Identity (Just sk) (toPublic sk))
-- ...
prettyPrint :: Identity -> String
prettyPrint (Identity _ pk) = "@" ++ encodedPubKey ++ ".ed25519"
  where encodedPubKey = BS.C8.unpack (encode $ BS.pack $ BA.unpack pk)


parseIdentity :: String -> Maybe Identity
parseIdentity str = if "@" `List.isPrefixOf` str && ".ed25519" `List.isSuffixOf` str then
                      let encodedPubKeyStr = List.take (List.length str - 9) . List.drop 1 $ str in
                      let encodedPubKey = decode $ BS.C8.pack encodedPubKeyStr in
                      case encodedPubKey of
                        Right encodedPubKey' -> case publicKey encodedPubKey' of
                                                  CryptoPassed pk' -> Just $ Identity Nothing pk'
                                                  CryptoFailed _   -> Nothing
                        Left _ -> Nothing
                    else
                      Nothing
