module Examples () where
import AST (LProg, pretty)
import Parser (parseLaneProg)
import Eval
import Raw (trans)
import TC
import Err (reportErr)

parseAndTrans e = case parseLaneProg e of 
  Left err -> error (show err)
  Right prog -> let ltprog = trans prog in case typeCheck ltprog of 
    Nothing -> elimType ltprog
    Just err -> error (reportErr err)

check e v = runProg (parseAndTrans e) == v 

examples = 
  [ ("if true then 10 else 20", FinalInt 10)
  , ("(fun (x : Int) => x) 5", FinalInt 5)
  , ("let x : Int = 2 in x", FinalInt 2)
  , ("let f : Int -> Int = fun (f : Int) => f in f 5", FinalInt 5)
  , ("let x : Int -> Int = fun (a : Int) => a in let y : Int = 2 in x y", FinalInt 2)
  , ("(1 + 2) * 4 / 2 + 100 * 2 - 50 / 2", FinalInt 181)
  , ("(fun (x : Int) (y : Int) => x + y) 10 20", FinalInt 30)
  ]

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
  
showExamples = mapM_ showExample examples 
  