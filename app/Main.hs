module Main where

import Data.Maybe
import Data.Either
import Data.List
import Data.Foldable (traverse_)

import Data.Time.Clock.System

import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as BS (fromString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.ByteArray as BA

import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Monad
import qualified Control.Exception as E

import System.IO
import System.Directory

import qualified Crypto.Error             as C
import qualified Crypto.PubKey.Ed25519    as C.Ed25519
import qualified Crypto.PubKey.Curve25519 as C.Cu25519

import qualified Data.Aeson               as A

import qualified Network.Info              as N
import qualified Network.BSD               as N (getProtocolNumber)
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N(recv, sendTo, sendAll)

import SSB.Misc
import SSB.Network
import SSB.Identity
import SSB.Message
import SSB.Message.Contact
import SSB.Message.Post
import SSB.Feed

main :: IO ()
main = do
  --
  -- Attempt to load an Identity or create a new one if none exists.
  --
  home <- getHomeDirectory
  createDirectoryIfMissing True (home ++ "/.ssb-hs/")
  fileExists <- doesFileExist (home ++ "/.ssb-hs/secret")
  our_seckey <- if fileExists
    then do
      loaded_seckey <- BS.readFile (home ++ "/.ssb-hs/secret")
      C.throwCryptoErrorIO $ C.Ed25519.secretKey loaded_seckey
    else do
      gen_seckey <- C.Ed25519.generateSecretKey
      BS.writeFile (home ++ "/.ssb-hs/secret") $ BA.convert gen_seckey
      return gen_seckey
  -- Derive pubkey and identity from secret key
  let our_pubkey = C.Ed25519.toPublic our_seckey
  let our_identity = Identity {sk = Just our_seckey, pk = our_pubkey}
  -- Print
  putStrLn $ "Loaded identity: " ++ prettyPrint our_identity

  --
  -- Attempt to load our Feed or create a new one if none exists.
  --
  fileExists <- doesFileExist $ home ++ "/.ssb-hs/messages/" ++ prettyPrint our_identity
  our_feed <- if fileExists
    then loadFeed $ home ++ "/.ssb-hs/messages/" ++ prettyPrint our_identity
    else do
      let created_feed = newFeed our_identity
      writeFeed created_feed $ home ++ "/.ssb-hs/messages/" ++ prettyPrint our_identity
      return $ Just created_feed

  --
  -- Start network processes (Forked)
  --
  -- Broadcast identity to LAN
  forkIO $ broadcast our_pubkey
  -- TODO: Listen to broadcasts and init connection to peers
  -- Listen for incoming connections
  forkIO listenIncoming


  --
  -- Start CLI
  --
  case our_feed of
    Just f  -> do
      putStrLn "Feed loaded succesfully"
      putStrLn "Type `help` to display options."
      menu our_identity
    Nothing -> putStrLn "Error, our feed is corrupted"

------------------- menu --------------------------
menu :: Identity -> IO ()
menu i = do
  -- General Info
  home <- getHomeDirectory
  feed <- loadFeed $ home ++ "/.ssb-hs/messages/" ++ prettyPrint i
  -- Get Input
  input <- words <$> getLine

  -- Process Input
  unless (null input || isNothing feed) $ do
    when (head input == "help") help
    when (head input == "follow" && length input >= 2) $ (follow . fromJust $ feed) (input !! 1)
    unless (head input == "exit") $ menu i
  when (null input) $ do
    unless (isNothing feed) $ menu i
    when (isNothing feed) $
      putStrLn "Feed error"
  -- case choice of
  --       '0' -> return()
  --       '1' -> do 
  --         putStrLn "Who do you want to follow? Please write it below."
  --         friend <- getLine
  --         let idFriend = fromJust $ parseIdentity friend
  --         putStrLn "Send him a message, so it's easier to locate you"
  --         greeting <- getLine
  --         time <- getSystemTime
  --         let time_seconds = toInteger $ systemSeconds  time
  --         let contact_friend = Contact{contact = idFriend, following = Just True, blocking = Nothing, pub = Nothing, SSB.Message.Contact.name = Nothing}
  --         let message_friend = Message{SSB.Message.sequence = 0, previous = Nothing, author = id, timestamp = time_seconds, hash = BS.fromString "sha256", content = greeting, signature = Nothing}
  --         menu id
  --       '2' -> do
  --         createDirectoryIfMissing True (home ++ "/.ssb-hs/messages/")
  --         list <- listDirectory (home ++ "/.ssb-hs/messages")
  --         putStrLn . unlines $ list
  --         menu id
          

  --       '3' -> do
  --         putStr "Please enter a file: "
  --         file <- getLine
  --         contents <- BS.readFile file
  --         let decodeContents = (A.decodeStrict :: BS.ByteString -> Maybe (Message BS.ByteString)) contents 
  --         menu id

  --       '4' -> do
  --         --hSetBuffering stdout LineBuffering
  --         putStrLn "Type your message: "
  --         content <- getLine
  --         time <- getSystemTime
  --         let time_seconds = toInteger $ systemSeconds time
  --         let message = Message{SSB.Message.sequence = 0, previous = Nothing, author = id, timestamp = time_seconds, hash = BS.fromString "sha256", content = content, signature = Nothing};
  --         let verified_mess = signMessage message
  --         let text_content = T.pack content
  --         case verified_mess of
  --           Nothing -> putStrLn "Signature failed"
  --           Just m -> do
  --             writeFile (home ++ "/.ssb-hs/messages/"++ "messages.txt") content
  --             let post = Post{text = text_content, root = Nothing, branch = Nothing, reply = Nothing, channel = Nothing, mentions = Nothing, recps = Nothing}
  --             let post_mess = Message{SSB.Message.sequence = 0, previous = Nothing, author = id, timestamp = time_seconds, hash = BS.fromString "sha256", content = post, signature = Nothing};
  --             let verified_post = signMessage post_mess
  --             case verified_post of
  --               Nothing -> putStrLn "Post failed"
  --               Just m -> do
  --                 writeFile (home ++ "/.ssb-hs/messages/"++ "messages.txt") content
  --                 putStrLn "Post and message wrote succesfully!"
  --         return()
  --       _  -> do
  --         putStrLn "Por favor anota solo algún numero del menu"
  --         menu id

help :: IO ()
help = do
  putStrLn "Available commands:"
  putStrLn "exit"
  putStrLn "1: Follow a user."
  putStrLn "2: Show all the messages that you have."
  putStrLn "3: Read a message"
  putStrLn "4: Write a post."
  putStrLn "Choice an option: "

follow :: Feed -> String -> IO ()
follow f s = do
  home <- getHomeDirectory
  let to_follow = fromJust . parseIdentity $ s
  time <- (`div` 1000) . toInteger . systemNanoseconds <$> getSystemTime
  let c = Contact { contact   = to_follow
                  , following = Just True
                  , blocking = Just False
                  , pub = Nothing
                  , SSB.Message.Contact.name = Nothing}
  let m = Message { SSB.Message.sequence = 0
                  , previous = Nothing
                  , author = identity f
                  , timestamp = time
                  , hash = BS.fromString "sha256"
                  , content = c
                  , signature = Nothing}

  case m `appendTo` f of
    Just f' -> do
      writeFeed f' $ home ++ "/.ssb-hs/messages/" ++ (prettyPrint . identity $ f')
      putStrLn $ "Succesfully followed: " ++ s
    Nothing -> putStrLn "Error following"

---------------------------------------------------
