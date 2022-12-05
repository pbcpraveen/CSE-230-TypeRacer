{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Utils (
  welcomeMsg,
  fileName,
  delay,
  recvTimeout,
  initRanking,
  Client (..),
  mkClient,
  shuffle,
  generateCorpus,
  sortedClients
) where

import System.Random

import Data.Array.IO hiding (newArray)
import Data.Map (Map, toList)
import qualified Data.List as List
import Network.Socket

import Control.Monad

welcomeMsg :: String
welcomeMsg = "Welcome to TypeRacer!"

fileName :: String
fileName = "assets/words_alpha.txt"

delay :: Int
delay = 100000  -- 0.1 second

recvTimeout :: Int
recvTimeout = 10000  -- 0.01 second

initRanking :: Int
initRanking = 1000

initProgress :: Double
initProgress = -1

data Client = Client { 
  ranking     :: Int,
  progress    :: Double,
  target_sock :: Socket
}
  deriving (Eq, Show)

-- first compare the ranking then compare the progress
instance Ord Client where
  compare :: Client -> Client -> Ordering
  compare (Client r1 p1 _) (Client r2 p2 _) = case compare r1 r2 of
    EQ -> compare p2 p1  -- higher is better
    x  -> x

mkClient :: Socket -> Client
mkClient sock = Client initRanking initProgress sock

-- get list from dict and sort by clients and ignore the socketAddr
sortedClients :: Map SockAddr Client -> [(Client, SockAddr)]
sortedClients dict = List.sort (map (\(addr, client) -> (client, addr)) (toList dict))

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray m =  newListArray (1,m)

-- read the file and return first n shuffled words
generateCorpus :: Int -> IO String
generateCorpus n = do
  contents <- readFile fileName
  let unshuffled = List.words contents
  shuffled <- shuffle unshuffled
  return $ unwords (take n shuffled)
