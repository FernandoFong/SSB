module Main where

import Data.Foldable (traverse_)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as BS (fromString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteArray as BA
import qualified Control.Exception as E
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Monad (unless, forever, void)
import qualified Crypto.Error          as C
import qualified Crypto.PubKey.Ed25519 as C
import System.Directory (doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import Network.Info
import Network.BSD (getProtocolNumber)
import Network.Socket hiding (recv, sendTo)
import Network.Socket.ByteString (recv, sendTo, sendAll)

import SSB.Identity

-- Some network code adapted from https://github.com/haskell/network
-- License: BSD-3
main :: IO ()
main = do
  -- Attempt to load a secrete or create a new one otherwise
  home <- getHomeDirectory
  createDirectoryIfMissing True (home ++ "/.ssb-hs/")
  fileExists <- doesFileExist (home ++ "/.ssb-hs/secret")
  secKey <- if fileExists
    then do
      secret <- BS.readFile (home ++ "/.ssb-hs/secret")
      C.throwCryptoErrorIO $ C.secretKey secret
    else do
      secretKey <- C.generateSecretKey
      BS.writeFile (home ++ "/.ssb-hs/secret") $ BA.convert secretKey
      return secretKey
  -- Derive pubkey and identity from secret key
  let pubKey = C.toPublic secKey
  let identity = Identity {sk=Just secKey, pk = pubKey}
  -- Print
  putStrLn $ "Loaded identity: " ++ prettyPrint identity
  -- Start network processes
  withSocketsDo $ do
    -- Broadcast identity to LAN
    forkIO $ broadcast pubKey
    -- TODO: Listen to broadcasts and init connection to peers
    -- Listen for incoming connections
    addr <- resolve "8008"
    E.bracket (open addr) close loop

resolve port = do
  let hints = defaultHints {
                  addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 10
  return sock

loop sock = forever $ do
  (conn, peer) <- accept sock
  putStrLn $ "Connection from " ++ show peer
  void $ forkFinally (talk conn) (\_ -> close conn)

talk conn = do
  msg <- recv conn 1024
  unless (BS.null msg) $ do
    sendAll conn msg
    talk conn

-- Adapted from https://github.com/audreyt/network-multicast
-- License: CC-0
broadcast :: C.PublicKey -> IO ()
broadcast pubkey = do
  -- Local interfaces, excluding loopbacks.
  interfaces <- getNetworkInterfaces
  let addresses = filter (\i -> i `notElem` ["127.0.0.1", "0.0.0.0"]) $
                  fmap (show . ipv4) interfaces
  -- Message to broadcast according to sepcification: https://ssbc.github.io/scuttlebutt-protocol-guide/#local-network
  let messages = fmap (\i -> BS.fromString ("net:" ++ i ++ ":8008~shs:") `BS.append`
                             (B64.encode . BA.convert $ pubkey)) addresses
  -- Broadcast Socket
  let addr = SockAddrInet 8008 (tupleToHostAddress (255,255,255,255))
  prot <- getProtocolNumber "udp"
  sock <- socket AF_INET Datagram prot
  setSocketOption sock Broadcast 1
  -- Broadcast loop, repeat message each second.
  forever $ do
    traverse_ (\m -> sendTo sock m addr) messages
    threadDelay (1000 * 1000)

