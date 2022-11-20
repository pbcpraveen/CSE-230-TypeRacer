module Backend (server) where

import Network.Socket
import Network.Socket.ByteString (sendAll)
import qualified Data.ByteString.Char8 as BS  -- avoids putStrLn conflict

someFunc :: IO ()
someFunc = putStrLn "someFunc"

server :: IO ()
server = do
  sock <- socket AF_INET Stream 0   -- create socket
  setSocketOption sock ReuseAddr 1  -- closes the socket immediately so can be reused
  bind sock (SockAddrInet 1234 0)   -- 0 here means accept any IP
  listen sock 2                     -- wait for clients to connect. 2 for queue size
  loop sock                         -- now we have created the sock object, pass it into the main loop

loop :: Socket -> IO ()
loop sock = do
  conn <- accept sock               -- connection established
  handle conn                       -- handle here is the main logic
  loop sock                         -- recurse until ctrl+c

handle :: (Socket, SockAddr) -> IO ()
handle (sock, addr) = do
  print addr                        -- print the client addr
  sendAll sock $ BS.pack "Hi back!"
  close sock                        -- close for now for testing
