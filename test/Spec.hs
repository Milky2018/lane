module Main (main) where 

import Test.Hspec

import Eval (FinalVal (..))
import Interpreter (lanei)
import Val (LVal(..))
import Builtins (trueVal)

checkExample :: String -> FinalVal -> SpecWith ()
checkExample path expectedVal = it path $ do
  file <- readFile path
  lanei file `shouldBe` expectedVal 

main :: IO ()
main = hspec $ do 
    describe "eval" $ do
      checkExample "examples/fact.lane" (FinalVal (LValInt 120))
      checkExample "examples/oddeven.lane" (FinalVal trueVal)
      checkExample "examples/lambda.lane" (FinalVal (LValInt 2))
      checkExample "examples/hello.lane" (FinalVal (LValString "Hello World"))
      checkExample "examples/moreargs.lane" (FinalVal (LValInt 2))
      checkExample "examples/letrec.lane" (FinalVal (LValInt 120))
      checkExample "examples/multiletrec.lane" (FinalVal trueVal)
      checkExample "examples/enum.lane" (FinalVal (LValEnum "MyBool" "myTrue" []))
      checkExample "examples/enumargs.lane" (FinalVal (LValEnum "OptionInt" "some" [LValInt 1]))
      checkExample "examples/nat.lane" (FinalVal (LValEnum "Nat" "suc" [LValEnum "Nat" "suc" [LValEnum "Nat" "zero" []]]))
      checkExample "examples/cal.lane" (FinalVal (LValInt (-4)))
      checkExample "examples/codata1.lane" (FinalVal (LValEnum "List" "cons" [LValInt 1, LValEnum "List" "cons" [LValInt 1, LValEnum "List" "nil" []]]))
      checkExample "examples/lc.lane" (FinalVal (LValInt 10))
      checkExample "examples/forall.lane" (FinalVal (LValInt 10))