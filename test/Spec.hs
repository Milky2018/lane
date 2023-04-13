module Main (main) where 

import Test.Hspec

import Eval (FinalVal (..))
import Interpreter (lanei)

checkExample :: String -> FinalVal -> SpecWith ()
checkExample path expectedVal = it path $ do
  file <- readFile path
  lanei file `shouldBe` expectedVal 

main :: IO ()
main = hspec $ do 
    describe "eval" $ do
      checkExample "examples/fact.lane" (FinalInt 120)
      checkExample "examples/oddeven.lane" (FinalBool True)
      checkExample "examples/lambda.lane" (FinalInt 2)
      checkExample "examples/hello.lane" (FinalString "Hello World")

