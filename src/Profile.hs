module Profile (laneProfile) where
  
import Parser (parseLaneProg)
import Raw (trans)
import Ty (pretty)
import TC (typeCheck, elimType)
import Err (reportErr)
import Eval (runProg)

laneProfile :: String -> IO ()
laneProfile progText = do
  putStrLn "Lane profiling"
  putStrLn "=============="
  putStrLn "Parsing program, output Raw AST:"
  let rawAst = case parseLaneProg progText of 
        Left err -> error (show err)
        Right ast -> ast
  print rawAst
  putStrLn "---------------------------"
  putStrLn "Translating Raw AST to MT AST:"
  let mtAst = trans rawAst 
  print $ pretty mtAst
  putStrLn "---------------------------"
  putStrLn "Type checking MT AST:"
  let lprog = case typeCheck mtAst of 
        Nothing -> elimType mtAst
        Just err -> error (reportErr err)
  print $ pretty lprog
  putStrLn "---------------------------"
  putStrLn "Evaluation, output final value:"
  print $ runProg lprog
  putStrLn "---------------------------"
  putStrLn "Done"