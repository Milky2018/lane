module Interpreter (parseAndTrans, lanei) where

import AST (LProg)
import Parser (parseLaneProg)
import Eval ( FinalVal(..), runProg )
import Raw (trans)
import Infer ( typeCheck, elimTypeProg )
import Err ( pretty )

parseAndTrans :: String -> LProg
parseAndTrans e = case parseLaneProg e of
  Left err -> error (show err)
  Right prog -> let mtprog = trans prog in case typeCheck mtprog of 
    Nothing -> elimTypeProg mtprog
    Just err -> error (show (pretty err))

lanei :: String -> FinalVal 
lanei = runProg . parseAndTrans 
