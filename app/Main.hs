module Main (main) where

import System.Environment (getArgs)

import Interpreter (lanei)
import Profile (laneProfile)

main :: IO () 
main = do 
  args <- getArgs 
  case args of 
    [] -> putStrLn "Usage: lane profile|run files"
    (command : fileNames) -> do 
      files <- mapM readFile fileNames
      case command of 
        "profile" -> mapM_ laneProfile files
        "run" -> mapM_ (print . lanei) files
        _ -> putStrLn "Usage: lane <filename>"