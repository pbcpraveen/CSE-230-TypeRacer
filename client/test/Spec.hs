import Test.Hspec

import TypeRacer

testComputePercentage :: IO ()
testComputePercentage = do
  let target = "hello world"
  let actual = "hello123"
  let result = computePercentage target actual
  print result
  abs (result-0.45454545) `shouldSatisfy` (< 0.001)

testPartition :: IO ()
testPartition = do
  let target = "hello world"
  let actual = "hello123"
  let result = partition target actual
  print result
  result `shouldBe` zip "hello123 world" [0,0,0,0,0,1,1,1,2,2,2,2,2,2]

main :: IO ()
main = testComputePercentage >> testPartition
