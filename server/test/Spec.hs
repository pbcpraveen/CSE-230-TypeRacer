import Network.Socket
import Network.Socket.ByteString (recv)

import Test.Hspec

welcomeMsg :: String
welcomeMsg = "Welcome to TypeRacer!"

corpus :: String
corpus = "The quick brown fox jumps over the lazy dog"

-- delay :: Int
-- delay = 1000000  -- 1 second

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
  show corpus1 `shouldContain` corpus
  corpus2 <- recv sock2 1024
  show corpus2 `shouldContain` corpus

main :: IO ()
main = testServer
