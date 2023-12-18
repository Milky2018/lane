module Profile (laneProfile) where
  
import Parser (parseLaneProg)
import Raw (trans)
import Pretty ( pretty )
import TC (typeCheck, elimTypeProg)
import Err (reportErr)
import Eval (runProg)

laneProfile :: String -> IO ()
laneProfile progText = do
  putStrLn "Lane profiling"
  doubleSepLine
  putStrLn "Parsing program, output Raw AST:"
  let rawAst = case parseLaneProg progText of 
        Left err -> error (show err)
        Right ast -> ast
  print rawAst
  sepLine
  putStrLn "Translating Raw AST to MT AST:"
  let mtAst = trans rawAst 
  print $ pretty mtAst
  sepLine
  putStrLn "Type checking MT AST:"
  let lprog = case typeCheck mtAst of 
        Nothing -> elimTypeProg mtAst
        Just err -> error (reportErr err)
  print $ pretty lprog
  sepLine
  putStrLn "Evaluation, output final value:"
  print $ pretty $ runProg lprog
  sepLine
  putStrLn "Done"

sepLine :: IO ()
sepLine = putStrLn "---------------------------"

doubleSepLine :: IO ()
doubleSepLine = putStrLn "==========================="