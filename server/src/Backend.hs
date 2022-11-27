module Backend (server) where

import Prelude hiding (length)

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import Data.Map (Map, empty, adjust, insert, toList)
import qualified Data.ByteString.Char8 as BS  -- avoids putStrLn conflict

import Control.Monad.State
import Control.Concurrent (threadDelay)

import System.Timeout (timeout)

corpus :: String
corpus = "The quick brown fox jumps over the lazy dog"

delay :: Int
delay = 1000000  -- 1 second

recvTimeout :: Int
recvTimeout = 100000  -- 0.1 second

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
  results <- execStateT (gameLoop True) players  -- start the game loop
  print results

signal :: Socket -> String -> IO ()  -- function to send a message to a socket
signal sock msg = sendAll sock (BS.pack msg)

broadcastProgress :: Map SockAddr Client -> IO ()
broadcastProgress dict = helper (toList dict) ('$':show dict)
  where
    helper []               _       = return ()
    helper ((_, client):xs) content = do
      signal (target_sock client) content
      helper xs content

broadcastMsg :: Map SockAddr Client -> String -> IO ()
broadcastMsg dict msg = helper (toList dict)
  where
    helper [] = return ()
    helper ((_, client):xs) = do
      signal (target_sock client) msg
      helper xs

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
  maybeMsg <- lift (timeout recvTimeout (recv sock 1024))
  case maybeMsg of
    Nothing  -> receiveProgress xs  -- just ignore the client if they don't respond
    Just msg -> do
      let new_progress = read (BS.unpack msg) :: Double
      dict <- get  -- get old dict
      put (adjust (\(Client s _) -> Client s new_progress) addr dict)  -- update dict
      receiveProgress xs  -- move on

gameShouldContinue :: Map SockAddr Client -> Bool
gameShouldContinue dict = helper (toList dict)
  where
    helper [] = True
    helper ((_, Client _ prog):xs) = prog < 1 && helper xs

gameLoop :: Bool -> StateT (Map SockAddr Client) IO ()
gameLoop False = return ()
gameLoop True  = do
  dict <- get
  -- lift (print dict)
  receiveProgress (toList dict)
  updated_dict <- get
  lift (broadcastProgress updated_dict)
  lift (threadDelay delay)  -- this delay lightens the cpu load
  gameLoop (gameShouldContinue updated_dict)
