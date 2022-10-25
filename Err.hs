module Err where 
import Ty
import TAST (LTExpr, pretty, prettyExpr)

data LErr = 
    LErr String 
  | LTypeErr LTExpr LType LType 
  | LBug String 
  deriving (Show, Eq)

type LResult = Either LErr

reportErr :: LErr -> String
reportErr (LErr s) = "normal error: " ++ s
reportErr (LTypeErr e t1 t2) = "Type error: expression " ++ prettyExpr e ++ "expected " ++ show t1 ++ " but got " ++ show t2
reportErr (LBug s) = "bug found: " ++ s