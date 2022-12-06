import Test.Hspec

import TypeRacer

testPartition :: IO ()
testPartition = do
  let target = "hello world"
  let actual = "hello123"
  let (correct, wrong, rest) = partition target actual
  putStrLn correct
  putStrLn wrong
  putStrLn rest
  correct `shouldBe` "hello"
  wrong `shouldBe` "123"
  rest `shouldBe` " world"

main :: IO ()
main = testPartition
