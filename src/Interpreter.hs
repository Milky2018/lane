module Interpreter (parseAndTrans, lanei) where

import AST (LProg)
import Parser (parseLaneProg)
import Eval ( FinalVal(..), runProg )
import Raw (trans)
import TC ( typeCheck, elimType )
import Err (reportErr)

parseAndTrans :: String -> LProg
parseAndTrans e = case parseLaneProg e of
  Left err -> error (show err)
  Right prog -> let mtprog = trans prog in case typeCheck mtprog of 
    Nothing -> elimType mtprog
    Just err -> error (reportErr err)

lanei :: String -> FinalVal 
lanei = runProg . parseAndTrans 
