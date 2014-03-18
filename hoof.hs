{-# LANGUAGE DeriveDataTypeable #-}

import System.Environment
import System.IO
import System.Posix
import System.Console.CmdArgs
import System.Console.GetOpt
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.Error
import Control.Concurrent
import Network.Socket
import Network.Sendfile

main = withSocketsDo $ do
  HoofOptions {  path = path,
                count = count,
                 port = port } <- cmdArgsRun parseOptions

  startServer path count port

data HoofOptions = HoofOptions {
    path  :: String
  , count :: Int
  , port  :: Int } deriving (Show, Data, Typeable)

parseOptions = cmdArgsMode $ HoofOptions
  { path  = "hoof"  &= argPos 0 &= typ "PATH"
  , count = 1       &= name "c" &= help "Amount of possible downloads"
  , port  = 1337    &= name "p" &= help "Port to serve on" }
  &= summary "Hoof 0.1 - 20% cooler than woof."

startServer :: String -> Int -> Int -> IO ()
startServer path count port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
  listen sock count
  putStrLn $ "Listening on 127.0.0.1 port " ++ show port
  done <- newEmptyMVar
  serveFile sock path count done

serveFile :: Socket -> String -> Int -> MVar () -> IO ()
serveFile _ _ 0 _ = return ()
serveFile sock path count done = do
  (client, sockAddr) <- accept sock
  forkIO $ respond client done
  takeMVar done
  serveFile sock path (count - 1) done
  where
    response fs = [
        BS.pack $ "HTTP/1.1 200 OK\r\n"
      , BS.pack $ "Content-Length: " ++ (show fs) ++ "\r\n"
      , BS.pack $ "Content-Type: application/octet-stream\r\n\r\n" ]
    respond client done = do
      fileHandle <- openFile path ReadMode
      stat <- getFileStatus path
      peer <- getPeerName client
      putStrLn $ "Sending file to " ++ show peer
      -- what does the fourth parameter even do? i have no idea... O.O
      sendfileWithHeader client path EntireFile (return ()) (response $ fileSize stat)
      putMVar done ()
