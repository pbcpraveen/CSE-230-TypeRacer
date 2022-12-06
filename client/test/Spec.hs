import Test.Hspec

import TypeRacer

testLineBreaks :: IO ()
testLineBreaks = do
  let (a, b, c)    = (replicate 10 '0', replicate 10 '1', replicate 10 '2')
      (a', b', c') = insertLineBreaks (a, b, c)
  (a', b', c') `shouldBe` (a, b, c)
  let (longa, longb, longc) = (replicate 64 '0', replicate 64 '1', replicate 64 '2')
      (longa', longb', longc') = insertLineBreaks (longa, longb, longc)
  (longa', longb', longc') `shouldBe` (longa++"\n", longb++"\n", longc++"\n")

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
main = putStrLn "\n" >> testLineBreaks >> testPartition
