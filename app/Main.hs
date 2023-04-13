module Main (main) where

import System.Environment (getArgs)

import Interpreter (lanei)

main :: IO ()
main = do
  testFiles <- getArgs 
  files <- mapM readFile testFiles
  mapM_ (print . lanei) files 
