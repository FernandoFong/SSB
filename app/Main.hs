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
import Network.Info 
import Network.BSD (getProtocolNumber)
import Network.Socket
import Network.Socket.ByteString (recv, sendTo, sendAll)
import qualified Data.Time.Clock.System     as S
import Data.Time.Clock.System
import System.Directory
import qualified Data.Text as T
import System.IO
import Control.Exception (bracket)
import Data.List

import SSB.Misc
import SSB.Network
import SSB.Identity
import SSB.Message
import SSB.Message.Contact
import SSB.Message.Post

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
  void $ forkFinally (handshake conn >>= exchangeMessages) (\_ -> close conn)

------------------- menu --------------------------
menu :: Identity -> IO()
menu id = do 
  putStrLn "Please select an option:"
  putStrLn "0: If you want to exit."
  putStrLn "1: Follow a user."
  putStrLn "2: Show all the messages that you have."
  putStrLn "3: Read a message"
  putStrLn "4: Write a post."
  putStrLn "Choice an option: "
  choice <- getChar
  home <- getHomeDirectory
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
          menu id
        '2' -> do
          createDirectoryIfMissing True (home ++ "/.ssb-hs/messages/")
          list <- listDirectory (home ++ "/.ssb-hs/messages")
          putStrLn . unlines $ list
          menu id
          

        '3' -> do
          putStr "Please enter a file: "
          file <- getLine
          contents <- BS.readFile file
          let decodeContents = (A.decodeStrict :: BS.ByteString -> Maybe (Message BS.ByteString)) contents 
          menu id

        '4' -> do
          --hSetBuffering stdout LineBuffering
          putStrLn "Type your message: "
          content <- getLine
          time <- getSystemTime
          let time_seconds = toInteger $ systemSeconds time
          let message = Message{SSB.Message.sequence = 0, previous = Nothing, author = id, timestamp = time_seconds, hash = BS.fromString "sha256", content = content, signature = Nothing};
          let verified_mess = signMessage message
          let text_content = T.pack content
          case verified_mess of
            Nothing -> putStrLn "Signature failed"
            Just m -> do
              writeFile (home ++ "/.ssb-hs/messages/"++ "messages.txt") content
              let post = Post{text = text_content, root = Nothing, branch = Nothing, reply = Nothing, channel = Nothing, mentions = Nothing, recps = Nothing}
              let post_mess = Message{SSB.Message.sequence = 0, previous = Nothing, author = id, timestamp = time_seconds, hash = BS.fromString "sha256", content = post, signature = Nothing};
              let verified_post = signMessage post_mess
              case verified_post of
                Nothing -> putStrLn "Post failed"
                Just m -> do
                  writeFile (home ++ "/.ssb-hs/messages/"++ "messages.txt") content
                  putStrLn "Post and message wrote succesfully!"
          return()
        _  -> do
          putStrLn "Por favor anota solo algún numero del menu"
          menu id




---------------------------------------------------
