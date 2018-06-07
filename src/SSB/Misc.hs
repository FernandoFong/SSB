module SSB.Misc where

import Data.Char
import Data.Text.Encoding
import Data.ByteString
import Data.ByteString.UTF8
import Data.ByteArray as BA
import Crypto.Hash     as C
import Crypto.MAC.HMAC as C
import Data.Aeson

import Data.Time.Clock.System

import Test.QuickCheck

instance Arbitrary ByteString where
  arbitrary = fromString . Prelude.filter isAlpha . getPrintableString <$> (arbitrary :: Gen PrintableString)

instance Arbitrary DotNetTime where
  arbitrary = do
    time <- arbitrary :: Gen (Positive Integer)
    let sys_time = MkSystemTime {systemSeconds = fromInteger . getPositive $ time, systemNanoseconds = 0} in
      return DotNetTime {fromDotNetTime = systemToUTCTime sys_time}

instance ToJSON ByteString where
  toJSON bs = String $ decodeUtf8 bs

instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ \bs -> pure $ encodeUtf8 bs

calcHmac key msg = BA.convert (C.hmacGetDigest . C.hmac key $ msg :: C.Digest C.SHA512t_256)
