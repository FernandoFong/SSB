module Main where

import Data.Foldable (traverse_)
import Data.Either
import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as BS (fromString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteArray as BA
import qualified Control.Exception as E
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Monad 
import qualified Crypto.Error             as C
import qualified Crypto.PubKey.Ed25519    as C.Ed25519
import qualified Crypto.PubKey.Curve25519 as C.Cu25519
import qualified Data.Aeson               as A
import Data.Maybe
import System.Directory (doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import Network.Info 
import Network.BSD (getProtocolNumber)
import Network.Socket hiding (recv, sendTo)
import Network.Socket.ByteString (recv, sendTo, sendAll)
import qualified Data.Time.Clock.System     as S
import Data.Time.Clock.System

import SSB.Misc
import SSB.Identity
import SSB.Message
import SSB.Message.Contact

networkIdentifier = fromRight (BS.fromString "") . B64.decode . BS.fromString $
  "1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan/s="

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
      C.throwCryptoErrorIO $ C.Ed25519.secretKey secret
    else do
      secretKey <- C.Ed25519.generateSecretKey
      BS.writeFile (home ++ "/.ssb-hs/secret") $ BA.convert secretKey
      return secretKey
  -- Derive pubkey and identity from secret key
  let pubKey = C.Ed25519.toPublic secKey
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
    forkIO $ E.bracket (open addr) close loop 
    menu identity

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
  void $ forkFinally (initConnection conn) (\_ -> close conn)

initConnection conn = do
  -- Perform SSB Handshake
  -- Documentation: https://ssbc.github.io/scuttlebutt-protocol-guide/#handshake
  hmac_auth             <- recv conn 32
  peer_ephemeral_pubkey <- recv conn 32
  when (hmac_auth == calcHmac networkIdentifier peer_ephemeral_pubkey) $ do
    putStrLn "First step of handshake completed."
    our_ephemeral_seckey   <- C.Cu25519.generateSecretKey
    let our_ephemeral_pubkey = C.Cu25519.toPublic our_ephemeral_seckey
    let hmac_auth    = calcHmac networkIdentifier our_ephemeral_pubkey
    sendAll conn hmac_auth
    sendAll conn $ BA.convert our_ephemeral_pubkey
    establishedConnection conn

establishedConnection conn = do
  putStrLn "Connection established!"
  return ()

------------------- menu --------------------------
menu :: Identity -> IO()
menu id = do 
  putStrLn "Please select an option:"
  putStrLn "0: If you want to exit."
  putStrLn "1: Follow a user."
  putStrLn "2: Show all the messages that you have."
  putStrLn "3: Create a new message"
  putStr "Choice an option: "
  choice <- getChar
  case choice of
        '0' -> return()
        '1' -> do 
          putStrLn "Who do you want to follow? Please write it below."
          friend <- getLine
          let idFriend = fromJust $ parseIdentity friend
          putStrLn "Send him a message, so it's easier to locate you"
          greeting <- getLine
          time <- getSystemTime
          let time_seconds = toInteger $ systemSeconds  time
          let contact_friend = Contact{contact = idFriend, following = Just True, blocking = Nothing, pub = Nothing, SSB.Message.Contact.name = Nothing}
          let message_friend = Message{SSB.Message.sequence = 0, previous = Nothing, author = id, timestamp = time_seconds, hash = BS.fromString "sha256", content = greeting, signature = Nothing}
          return()
        --'2' ->
        '3' -> do
          putStr "Please enter a file: "
          file <- getLine
          contents <- BS.readFile file
          let decodeContents = (A.decodeStrict :: BS.ByteString -> Maybe (Message BS.ByteString)) contents 
          return()



---------------------------------------------------

-- Adapted from https://github.com/audreyt/network-multicast
-- License: CC-0
broadcast :: C.Ed25519.PublicKey -> IO ()
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
