module Backend (topServer) where

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import Data.Map (Map, empty, adjust, insert, toList)
import qualified Data.ByteString.Char8 as BS  -- avoids putStrLn conflict

import Control.Monad.State
import Control.Concurrent (threadDelay)

import System.Timeout (timeout)
import Text.Read (readMaybe)
import System.Environment (getArgs)

import Utils

topServer :: IO ()
topServer = do
  args <- getArgs
  case args of
    [num, len] -> do
      let nPlayers  = read num :: Int
      let lenCorpus = read len :: Int
      corpus <- generateCorpus lenCorpus
      server nPlayers corpus
    _          -> putStrLn usageMsg

server :: Int -> String -> IO ()
server nPlayers corpus = do
  sock <- socket AF_INET Stream 0                       -- create socket
  setSocketOption sock ReuseAddr 1                      -- closes the socket immediately so can be reused
  bind sock (SockAddrInet 1234 0)                       -- 0 here means accept any IP
  listen sock nPlayers                                  -- wait for clients to connect. 2 for queue size
  players <- execStateT (joinGame sock nPlayers) empty  -- wait for nPlayers to join
  broadcastMsg players corpus                           -- send the text to type
  results <- execStateT (gameLoop True) players         -- start the game loop
  print results

signal :: Socket -> String -> IO ()  -- function to send a message to a socket
signal sock msg = sendAll sock (BS.pack msg)

padCenter :: Int -> String -> String
padCenter n str | n <= length str = str
                | otherwise       = replicate frontLen ' ' ++ str ++ replicate backLen ' '
  where
    padLen = n - length str
    frontLen = padLen `div` 2
    backLen = padLen - frontLen

-- take after the first colon
addrToGuestName :: SockAddr -> String
addrToGuestName addr = padCenter 10 $ "user" ++ dropWhile (/= ':') (show addr)

makePersonalizedMsg :: (Client, SockAddr) -> [(Client, SockAddr)] -> String
makePersonalizedMsg (Client rank prog _, addr) sortedList = foldl step initAcc filteredList
  where
    filteredList = filter (\(_, otherAddr) -> otherAddr /= addr) sortedList
    step acc (Client otherRank otherProg _, otherAddr) = acc ++ ('|' : addrToGuestName otherAddr ++ "," ++ show otherRank ++ ","  ++ show otherProg)
    initAcc = padCenter 10 "You" ++ ","  ++ show rank ++ ","  ++ show prog

-- for each client, send personalized progress msg with their own name switched to "you"
-- for other users, their name is "user<port number>"
-- each client progress is encoded in a 3 typle (name, progress, ranking) where ranking is an integer -1 if not done 
broadcastProgress :: Map SockAddr Client -> IO ()
broadcastProgress dict = helper (sortedClients dict) (sortedClients dict)
  where
    helper []              _          = return ()
    helper (x@(cli, _):xs) sortedList = do
      let msg = makePersonalizedMsg x sortedList
      signal (target_sock cli) msg
      helper xs sortedList

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
  lift $ putStrLn ("waiting for " ++ show n ++ " more players")
  (client_sock, addr) <- lift (accept sock)
  lift (print addr)
  dict <- get
  put (insert addr (mkClient client_sock) dict)  -- add the client to the map
  lift (signal client_sock welcomeMsg)
  joinGame sock (n-1)

receiveProgress :: [(Client, SockAddr)] -> Int -> StateT (Map SockAddr Client) IO ()
receiveProgress [] _    = return ()
receiveProgress ((Client rank _ sock, addr):xs) counter
  | rank /= initRanking = receiveProgress xs (rank+1)
  | otherwise           = do
    maybeMsg <- lift (timeout recvTimeout (recv sock 1024))
    case maybeMsg of
      Nothing  -> receiveProgress xs counter -- just ignore the client if they don't respond
      Just msg -> do
        if BS.null msg then receiveProgress xs counter else do
          lift (print msg)
          let msg' = last (BS.split '|' msg)
          lift (print msg')
          dict <- get  -- get old dict
          case readMaybe (BS.unpack msg') :: Maybe Double of
            Nothing        -> receiveProgress xs counter
            Just 1         -> do
              put (adjust (\client -> client {ranking = counter, progress = 1}) addr dict)
              receiveProgress xs (counter+1)  -- increment ranking counter
            Just new_prog  -> do
              put (adjust (\client -> client {progress = new_prog}) addr dict)
              receiveProgress xs counter

gameShouldContinue :: Map SockAddr Client -> Bool
gameShouldContinue dict = helper (toList dict)
  where
    helper [] = False
    helper ((_, Client _ prog _):xs) = prog < 1 || helper xs

gameLoop :: Bool -> StateT (Map SockAddr Client) IO ()
gameLoop False = do
  dict <- get
  lift (broadcastMsg dict gameOverMsg)
gameLoop True  = do
  dict <- get
  -- lift (print dict)
  receiveProgress (sortedClients dict) 1
  updated_dict <- get
  lift (broadcastProgress updated_dict)
  lift (threadDelay delay)  -- this delay lightens the cpu load
  gameLoop (gameShouldContinue updated_dict)
