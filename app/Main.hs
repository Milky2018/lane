module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)

import Lib (lanei)

main :: IO ()
main = do
  testFiles <- getArgs 
  files <- mapM readFile testFiles
  mapM_ (print . lanei) files 
