module Err (LErr (..), LResult, reportErr) where 
import Ty
import TAST (MTExpr)

data LErr = 
    LErr String 
  | LTErr MTExpr LType LType 
  | LBug String 
  | LMultiErr [LErr]
  deriving (Show, Eq)

type LResult = Either LErr

reportErr :: LErr -> String
reportErr (LErr s) = "normal error: " ++ s
reportErr (LTErr e t1 t2) = "Type error: expression " ++ pretty e ++ " expected " ++ pretty t1 ++ " but got " ++ pretty t2
reportErr (LBug s) = "bug found: " ++ s
reportErr (LMultiErr errs) = unlines $ map reportErr errs