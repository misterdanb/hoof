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

import Data.Time
import System.CPUTime

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
  
  -- initialize the threads mvar with the amount of file transfers
  -- possible; this mvar assures that the main thread only terminates,
  -- if all file transfers are finished
  threads <- newMVar count
  
  -- initialize the done mvar with empty, so the main thread blocks
  -- when calling takeMVar 
  done <- newEmptyMVar
  
  -- start serving the file
  serveFile sock path threads done count

serveFile :: Socket -> String -> MVar Int -> MVar () -> Int -> IO ()
serveFile _ _ _ done 0 =
  -- wait until done is not empty anymore, in other words untile all
  -- threads have finished
  takeMVar done
serveFile sock path threads done count = do
  -- accept clients
  (client, addr) <- accept sock
  
  -- create a thread to transfer the file concurrently
  forkIO $ respond client threads done
  
  -- loop this method until the amount of files specified by count
  -- has been served
  serveFile sock path threads done (count - 1)
  where
    response fs = [
        BS.pack $ "HTTP/1.1 200 OK\r\n"
      , BS.pack $ "Content-Length: " ++ (show fs) ++ "\r\n"
      , BS.pack $ "Content-Type: application/octet-stream\r\n\r\n" ]
    respond client threads done = do
      -- open the file
      fileHandle <- openFile path ReadMode
      
      -- retrieve file information
      stat <- getFileStatus path
      
      -- retrieve client information
      peer <- getPeerName client
      
      putStrLn $ "Sending file to " ++ show peer
      
      -- what does the fourth parameter even do? i have no idea... O.O
      sendfileWithHeader client path EntireFile (putStrLn "test") (response $ fileSize stat)
      
      -- delay for a second or so, because sendfileWithHeader does
      -- not finish properly though it is a blocking call; i have no
      -- idea why this has to be
      threadDelay 1000000
      
      -- take the threads mvar for mutual exclusion; other take
      -- operations will block until threads is not empty anymore
      leftThreads <- takeMVar threads
      
      -- put the threads mvar with decremented value because this
      -- thread will finish
      putMVar threads (leftThreads - 1)
      
      -- if there are no threads left serving files, tell the main
      -- thread that it may finish, otherwise do nothing
      if leftThreads - 1 == 0 
      then putMVar done ()
      else return ()
