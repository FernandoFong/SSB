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
  createDirectoryIfMissing True (home ++ "/.ssb-hs/messages/")
  fileExists <- doesFileExist $ home ++ "/.ssb-hs/messages/" ++ prettyPrint our_identity
  our_feed <- if fileExists
    then loadFeed $ home ++ "/.ssb-hs/messages/" ++ (escape . prettyPrint$ our_identity)
    else do
      let created_feed = newFeed our_identity
      writeFeed created_feed $ home ++ "/.ssb-hs/messages/" ++ (escape . prettyPrint $ our_identity)
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
  feed' <- loadFeed $ home ++ "/.ssb-hs/messages/" ++ (escape . prettyPrint $ i)
  let feed = (fromJust feed') {identity = i}
  -- Get Input
  putStr "> "
  hFlush stdout
  input <- words <$> getLine
  putStrLn ""

  -- Process Input
  unless (null input) $ do
    case head input of
      "help" -> help
      "follow" -> do
        guard (length input >= 2)
        follow feed (input !! 1)
      "write" -> do
        guard (length input >= 2)
        writePost feed (unlines . tail $ input)
      "list" -> do
        list <- listDirectory (home ++ "/.ssb-hs/messages")
        putStrLn "This are the available feeds: "
        putStrLn "----------"
        putStrLn . unlines $ list
      "exit" -> mempty
      _ -> putStrLn "Unrecognized input!"
    putStrLn ""
    -- Loop if not asked to exit
    unless (head input == "exit") $ menu i

  -- Process empty input
  when (null input) $ menu i


  --       '3' -> do
  --         putStr "Please enter a file: "
  --         file <- getLine
  --         contents <- BS.readFile file
  --         let decodeContents = (A.decodeStrict :: BS.ByteString -> Maybe (Message BS.ByteString)) contents 
  --         menu id

help :: IO ()
help = do
  putStrLn "Available commands:"
  putStrLn "----------"
  putStrLn "list \t\t | Lists all available feeds"
  putStrLn "follow [id] \t | Follows another identity"
  putStrLn "write [text] \t | Writes a post [Can't be deleted]"
  putStrLn "exit \t\t | Exits the program"

follow :: Feed -> String -> IO ()
follow f s = do
  let to_follow = fromJust . parseIdentity $ s
  let c = Contact { contact   = to_follow
                  , following = Just True
                  , blocking = Just False
                  , pub = Nothing
                  , SSB.Message.Contact.name = Nothing}
  writeMessage f c

writePost :: Feed -> String -> IO ()
writePost f s = do
  let c = Post { text = T.pack s
               , root     = Nothing
               , branch   = Nothing
               , reply    = Nothing
               , channel  = Nothing
               , mentions = Nothing
               , recps    = Nothing
               }
  writeMessage f c


writeMessage :: A.ToJSON a => Feed -> a -> IO ()
writeMessage f c = do
  home <- getHomeDirectory
  time <- messageCurrentTime
  let m = Message { SSB.Message.sequence = 0
                  , previous = Nothing
                  , author = identity f
                  , timestamp = time
                  , hash = BS.fromString "sha256"
                  , content = c
                  , signature = Nothing
                  }
  case m `appendTo` f of
    Just f' -> writeFeed f' $ home ++ "/.ssb-hs/messages/" ++ (escape. prettyPrint . identity $ f')
    Nothing -> putStrLn "Error following"
