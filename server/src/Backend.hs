module Backend (server) where

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import Data.Map (Map, empty, fromList, insert, toList)
import qualified Data.ByteString.Char8 as BS  -- avoids putStrLn conflict

import Control.Monad.State

data Client = Client { 
  target_sock :: Socket, 
  progress    :: Double
}
  deriving (Show)

server :: IO ()
server = do
  sock <- socket AF_INET Stream 0         -- create socket
  setSocketOption sock ReuseAddr 1        -- closes the socket immediately so can be reused
  bind sock (SockAddrInet 1234 0)         -- 0 here means accept any IP
  listen sock 2                           -- wait for clients to connect. 2 for queue size
  players <- execStateT (joinGame sock 2) empty  -- now we have created the sock object, pass it into the main loop
  -- execStateT (gameLoop sock) players
  print players

signal :: Socket -> String -> IO ()
signal sock msg = sendAll sock (BS.pack msg)

broadcastProgress :: Map SockAddr Client -> IO ()
broadcastProgress dict  = helper (toList dict)
  where
    helper [] = return ()
    helper ((_, client):xs) = do
      signal (target_sock client) (show (progress client))
      helper xs

joinGame :: Socket -> Int -> StateT (Map SockAddr Client) IO ()
joinGame _ 0 = return ()
joinGame sock n = do
  (client_sock, addr) <- lift (accept sock)
  -- handle (client_sock, addr)
  lift (print addr)                        -- print the client addr
  dict <- get
  put (insert addr (Client {target_sock = client_sock, progress = -1}) dict)  -- add the client to the map
  lift (signal client_sock "Welcome to TypeRacer!")
  updated_dict <- get
  lift (broadcastProgress updated_dict)
  -- byteStr <- lift (recv sock 1024)
  -- lift (print byteStr)
  -- dict <- get
  -- _ <- put (insert addr (-1) dict)
  joinGame sock (n-1)

-- loop :: Socket -> StateT (Map SockAddr Client) IO ()
-- loop sock = do
--   conn <- lift (accept sock)  -- connection established
--   handle conn                       -- handle here is the main logic
--   loop sock                         -- recurse until ctrl+c

-- handle :: (Socket, SockAddr) -> StateT (Map SockAddr Client) IO ()
-- handle conn@(sock, addr) = do
--   lift (print addr)                        -- print the client addr
--   byteStr <- lift (recv sock 1024)
--   lift (print byteStr)
--   lift (sendAll sock (BS.pack "Hi back!"))
--   byteStr2 <- lift (recv sock 1024)
--   lift (print byteStr2)
--   dict <- get
--   put (insert conn (-1) dict)
--   lift (sendAll sock (BS.pack "Hi back!"))
--   -- put (adjust (\k -> a -> a)  (Map k a))
--   -- lift (close sock)                        -- close for now for testing
