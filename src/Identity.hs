module Identity where

import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Base64
import Data.ByteArray
import Crypto.PubKey.Ed25519

data Identity = Identity (Maybe SecretKey) PublicKey

instance Show Identity where
  -- | show Identity. @{base64(PublicKey)}.ed25519
  -- | Example: @+DOl55sJo1wBqWtRuI3otubyE4M24q34X4wikX+7BXg=.ed25519
  -- >>> sk <- generateSecretKey
  -- >>> (Identity (Just sk) (toPublic sk))
  -- ...
  show (Identity _ pk) = "@" ++
                         Data.ByteString.Char8.unpack (encode $ Data.ByteString.pack $ Data.ByteArray.unpack pk) ++
                         ".ed25519"
