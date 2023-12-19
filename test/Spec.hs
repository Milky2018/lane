module Main (main) where 

import Test.Hspec

import Eval (FinalVal (..))
import Interpreter (lanei)
import Val (LVal(..))

checkExample :: String -> FinalVal -> SpecWith ()
checkExample path expectedVal = it path $ do
  file <- readFile path
  lanei file `shouldBe` expectedVal 

main :: IO ()
main = hspec $ do 
    describe "eval" $ do
      checkExample "examples/fact.lane" (FinalVal (LValInt 120))
      checkExample "examples/oddeven.lane" (FinalVal (LValBool True))
      checkExample "examples/lambda.lane" (FinalVal (LValInt 2))
      checkExample "examples/hello.lane" (FinalVal (LValString "Hello World"))
      checkExample "examples/struct.lane" (FinalVal (LValInt (-2)))
      checkExample "examples/moreargs.lane" (FinalVal (LValInt 2))
      checkExample "examples/letrec.lane" (FinalVal (LValInt 120))
      checkExample "examples/multiletrec.lane" (FinalVal (LValBool True))
      checkExample "examples/enum.lane" (FinalVal (LValEnum "MyBool" "myTrue" []))
      checkExample "examples/enumargs.lane" (FinalVal (LValEnum "OptionInt" "some" [LValInt 1]))