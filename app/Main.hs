module Main (main) where

import System.Environment (getArgs)

import Interpreter (lanei)
import Profile (laneProfile)
import Prettyprinter (pretty)

main :: IO () 
main = do 
  args <- getArgs 
  case args of 
    [] -> putStrLn "Usage: lane trace|run files"
    (command : fileNames) -> do 
      files <- mapM readFile fileNames
      case command of 
        "trace" -> mapM_ laneProfile files
        "run" -> mapM_ (print . pretty . lanei) files
        _ -> putStrLn "Usage: lane trace|run files"