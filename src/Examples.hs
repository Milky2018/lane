module Examples (showExamples) where
import AST (LProg)
import Parser (parseLaneProg)
import Eval ( FinalVal(FinalInt), runProg )
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
  [ ("def main => 1", FinalInt 1)
  , ("def main => 1 + 2", FinalInt 3)
  , ("def main => if true then 10 else 20", FinalInt 10)
  , ("def main => (fun (x : Int) => x) 5", FinalInt 5)
  , ("def main => let x : Int = 2 in x", FinalInt 2)
  , ("def main => let f : Int -> Int = fun (f : Int) => f in f 5", FinalInt 5)
  , ("def main => let x : Int -> Int = fun (a : Int) => a in let y : Int = 2 in x y", FinalInt 2)
  , ("def main => (1 + 2) * 4 / 2 + 100 * 2 - 50 / 2", FinalInt 181)
  , ("def main => (fun (x : Int) (y : Int) => x + y) 10 20", FinalInt 30)
  , ("def main => let x : Int = 1, y : Int = 2 in x + y", FinalInt 3)
  , ("def f => fun (x : Int) => x + 5; def main => f 10", FinalInt 15)
  , ("def f (x : Int) => x + 5; def main => f 10", FinalInt 15)
  , ("def main => f 10; def f (x : Int) => x + 5", FinalInt 15)
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
  