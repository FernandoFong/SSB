module Main where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.UTF8  as BS (fromString)
import qualified Data.ByteArray as BA
import qualified Control.Exception as E
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Monad (unless, forever, void)
import qualified Crypto.Error          as C
import qualified Crypto.PubKey.Ed25519 as C
import System.Directory (doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import Network.BSD (getProtocolNumber)
import Network.Socket hiding (recv, sendTo)
import Network.Socket.ByteString (recv, sendTo, sendAll)

import SSB.Identity

main :: IO ()
main = do
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
  let pubKey = C.toPublic secKey
  let identity = Identity {sk=Just secKey, pk = pubKey}
  putStrLn $ prettyPrint identity
  withSocketsDo $ do
    forkIO $ broadcast pubKey
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

broadcast pubkey = do
  let addr = SockAddrInet 8008 (tupleToHostAddress (255,255,255,255))
  prot <- getProtocolNumber "udp"
  sock <- socket AF_INET Datagram prot
  setSocketOption sock Broadcast 1
  forever $ do
    sendTo sock (BA.convert pubkey) addr
    threadDelay (1000 * 1000)
