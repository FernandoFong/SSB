module SSB.Misc where

import Data.Char
import Data.Time.Clock.System


import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteArray as BA

import qualified Crypto.Hash     as C
import qualified Crypto.MAC.HMAC as C

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A

import Test.QuickCheck

instance Arbitrary BS.ByteString where
  arbitrary = BS.fromString . Prelude.filter isAlpha . getPrintableString <$> (arbitrary :: Gen PrintableString)

instance A.ToJSON BS.ByteString where
  toJSON bs = A.String $ T.decodeUtf8 bs

instance A.FromJSON BS.ByteString where
  parseJSON = A.withText "ByteString" $ return . T.encodeUtf8

calcHmac key msg = BA.convert (C.hmacGetDigest . C.hmac key $ msg :: C.Digest C.SHA512t_256)

sha256 :: BS.ByteString -> C.Digest C.SHA256
sha256 = C.hash

--
-- | Default encoding following the Protocol Documentation as closely as possible
--
encode :: A.ToJSON a => a -> BS.ByteString
encode = head . encode'

--
-- | Since there are many messages IRL with different orders this helps verification attempts.
--
encode' :: A.ToJSON a => a -> [BS.ByteString]
encode' m = fmap (\c -> BS.toStrict . A.encodePretty' c $ m) confPPSSB'

confPPSSB' = fmap (\order -> A.Config { A.confIndent = A.Spaces 2
                                     , A.confCompare = order
                                     , A.confNumFormat = A.Generic
                                     , A.confTrailingNewline = False
                                     }) reasonableMessageOrders
  where reasonableMessageOrders = do
          message <- messageOrders
          contact <- contactOrders
          mention <- mentionOrders
          post    <- postOrders
          return . A.keyOrder . concatMap (fmap T.pack) $ [message, contact, mention, post]
        messageOrders = [["previous", "author", "sequence", "timestamp", "hash", "content", "type"],
                         ["previous", "sequence", "author", "timestamp", "hash", "content", "type"]]
        contactOrders = [["contact", "following", "blocking", "pub", "name"]]
        mentionOrders = [["link", "name"]]
        postOrders    = [["root", "branch", "reply", "channel", "rcps", "text", "mentions"]]

escape :: String -> String
escape [] = []
escape (x:xs) | x == '/' = '\\' : escape xs
              | otherwise = x : escape xs
