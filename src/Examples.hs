module Examples (showExamples, parseAndTrans) where
import AST (LProg)
import Parser (parseLaneProg)
import Eval ( FinalVal(..), runProg )
import Raw (trans)
import TC ( typeCheck, elimType )
import Err (reportErr)
import Pretty (Pretty(..))

parseAndTrans :: String -> LProg
parseAndTrans e = case parseLaneProg e of
  Left err -> error (show err)
  Right prog -> let mtprog = trans prog in case typeCheck mtprog of 
    Nothing -> elimType mtprog
    Just err -> error (reportErr err)

-- check e v = runProg (parseAndTrans e) == v 

examples :: [(String, FinalVal)]
examples =
  [ ("def fact (n : Int) : Int => "
  ++ "  if n == 0 then 1 else n * (fact (n - 1)); "
  ++ "def main : Int => fact 4"
  , FinalInt 24)
  , ("def odd (n : Int) : Bool => "
  ++ "  if n == 0 then false else even (n - 1); "
  ++ "def even (n : Int) : Bool => "
  ++ "  if n == 0 then true else odd (n - 1); "
  ++ "def main : Bool => odd 5"
  , FinalBool True)
  ]

showExample :: (String, FinalVal) -> IO ()
showExample (ex, expected) = do
  let prog = parseAndTrans ex
  let evaled = runProg prog
  putStr "\n--- "
  if evaled == expected
  then putStrLn "pass"
  else putStrLn "!!!"
  putStrLn $ pretty prog
  putStrLn "evaluated result: "
  print evaled
  putStrLn "expected result: "
  print expected

showExamples :: IO ()
showExamples = mapM_ showExample examples
