import Network.Socket
import Network.Socket.ByteString (recv)

import Test.Hspec

import Utils
import Data.List (sort)

-- create two clients and connect to the server, expecting to receive the corpus
testServer :: IO ()
testServer = do
  sock1 <- socket AF_INET Stream 0
  sock2 <- socket AF_INET Stream 0
  connect sock1 (SockAddrInet 1234 0)
  welcome1 <- recv sock1 1024
  show welcome1 `shouldContain` welcomeMsg
  connect sock2 (SockAddrInet 1234 0)
  welcome2 <- recv sock2 1024
  show welcome2 `shouldContain` welcomeMsg
  corpus1 <- recv sock1 1024
  length (words (show corpus1)) `shouldBe` 10
  corpus2 <- recv sock2 1024
  length (words (show corpus2)) `shouldBe` 10
  corpus1 `shouldBe` corpus2

testShuffle :: IO ()
testShuffle = do
  let xs = [-1000..1000] :: [Int]
  ys <- shuffle xs
  sort ys `shouldBe` xs

main :: IO ()
main = do
  testShuffle
  testServer
