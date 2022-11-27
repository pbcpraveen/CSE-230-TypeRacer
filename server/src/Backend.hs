module Backend (server) where

import Prelude hiding (length)

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import Data.Map (Map, empty, adjust, insert, toList)
import Data.ByteString (length)
import qualified Data.ByteString.Char8 as BS  -- avoids putStrLn conflict

import Control.Monad.State
import Control.Concurrent (threadDelay)

corpus :: String
corpus = "The quick brown fox jumps over the lazy dog"

delay :: Int
delay = 1000000  -- 0.1 second

data Client = Client { 
  target_sock :: Socket, 
  progress    :: Double
}
  deriving (Show)

server :: IO ()
server = do
  sock <- socket AF_INET Stream 0                -- create socket
  setSocketOption sock ReuseAddr 1               -- closes the socket immediately so can be reused
  bind sock (SockAddrInet 1234 0)                -- 0 here means accept any IP
  listen sock 2                                  -- wait for clients to connect. 2 for queue size
  players <- execStateT (joinGame sock 2) empty  -- now we have created the sock object, pass it into the main loop
  broadcastMsg players corpus                    -- send the text to type
  -- setSocketOption sock RecvTimeOut delay         -- set a low timeout for recv so we can loop through players
  results <- execStateT (gameLoop True) players  -- start the game loop
  print results

signal :: Socket -> String -> IO ()
signal sock msg = sendAll sock (BS.pack msg)

broadcastProgress :: Map SockAddr Client -> IO ()
broadcastProgress dict  = helper (toList dict)
  where
    helper [] = return ()
    helper ((_, client):xs) = do
      signal (target_sock client) (show (progress client))
      helper xs

broadcastMsg :: Map SockAddr Client -> String -> IO ()
broadcastMsg dict msg = helper (toList dict)
  where
    helper [] = return ()
    helper ((_, client):xs) = do
      signal (target_sock client) msg
      helper xs

-- setLowRecvTimeout :: Map SockAddr Client -> IO ()
-- setLowRecvTimeout dict = helper (toList dict)
--   where
--     helper [] = return ()
--     helper ((_, Client sock _):xs) = do
--       setSocketOption sock RecvTimeOut delay
--       helper xs

joinGame :: Socket -> Int -> StateT (Map SockAddr Client) IO ()
joinGame _ 0 = do
  lift (threadDelay delay)  -- wait for 0.1 second so msgs don't get mixed up
  return ()
joinGame sock n = do
  (client_sock, addr) <- lift (accept sock)
  lift (print addr)
  dict <- get
  put (insert addr (Client {target_sock = client_sock, progress = -1}) dict)  -- add the client to the map
  lift (signal client_sock "Welcome to TypeRacer!")
  joinGame sock (n-1)

receiveProgress :: [(SockAddr, Client)] -> StateT (Map SockAddr Client) IO ()
receiveProgress [] = return ()
receiveProgress ((addr, Client sock _):xs) = do
  msg <- lift (recv sock 1024)
  if length msg == 0
    then receiveProgress xs
    else let new_progress = read (BS.unpack msg) :: Double in do
      dict <- get
      put (adjust (\(Client s _) -> Client s new_progress) addr dict)
      receiveProgress xs

gameLoop :: Bool -> StateT (Map SockAddr Client) IO ()
gameLoop False = return ()
gameLoop True  = do
  lift (putStrLn "looping...")
  dict <- get
  receiveProgress (toList dict)
  updated_dict <- get
  lift (broadcastProgress updated_dict)
  lift (threadDelay delay)
  gameLoop True
