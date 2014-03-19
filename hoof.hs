{-# LANGUAGE DeriveDataTypeable #-}

import System.IO
import System.Posix
import System.Console.CmdArgs
import System.Console.GetOpt
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import Network.Socket
import Network.Sendfile

data Hoof =
  Hoof { path  :: String
       , count :: Int
       , port  :: Int
       , ip    :: Int } deriving (Show, Data, Typeable)

main = withSocketsDo $ do
  -- parse the command line options
  Hoof { path = path
       , count = count
       , port = port
       , ip = ip } <- cmdArgsRun parseOptions
  -- start the server with the given options
  startServer path count port ip

parseOptions =
  -- these are the possible cmmoand line options
  cmdArgsMode $ Hoof { path  = "hoof" &= argPos 0 &= typ "PATH"
                     , count = 1      &= name "c" &= help "Amount of possible downloads"
                     , port  = 1337   &= name "p" &= help "Port to serve on"
                     , ip    = 4      &= name "i" &= help "IP version to be used" }
                     &= summary "Hoof 0.1 - 20% cooler than woof."

startServer :: String -> Int -> Int -> Int -> IO ()
startServer path count port 4 = do
  -- create the IPv4 socket
  sock <- socket AF_INET Stream 0
  
  -- configure the socket to reuse address
  setSocketOption sock ReuseAddr 1
  
  -- bind the socket to the given port and allow any addresses to
  -- connect
  bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
  
  putStrLn $ "Listening on 127.0.0.1 port " ++ show port
  
  -- start listening
  listenOnSocket path count port sock
startServer path count port 6 = do
  -- create the IPv6 socket
  sock <- socket AF_INET6 Stream 0
  
  -- set socket to reuse address
  setSocketOption sock ReuseAddr 1
  
  -- bind the socket to the given port and allow any addresses to
  -- connect
  bindSocket sock (SockAddrInet6 (fromIntegral port) 0 iN6ADDR_ANY 0)
  
  putStrLn $ "Listening on ::1 port " ++ show port
  
  -- start listening
  listenOnSocket path count port sock
startServer _ _ _ _ =
  putStrLn "This IP version is not supported."
  
listenOnSocket :: String -> Int -> Int -> Socket -> IO ()
listenOnSocket path count port sock = do
  listen sock count
  
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
      header fs =
        [ BS.pack $ "HTTP/1.1 200 OK\r\n"
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
        sendfileWithHeader client path EntireFile (putStrLn "test") (header $ fileSize stat)
        
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
        if leftThreads - 1 == 0 then putMVar done ()
        else return ()
