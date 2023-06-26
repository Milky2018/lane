module Err (LErr (..), LResult, reportErr) where 
import Ty ( pretty, LTypeVal )
import TAST (TVExpr)

data LErr = 
    LErr String 
  | LTErr TVExpr LTypeVal LTypeVal 
  | LTFiledNotFound String
  | LBug String 
  | LMultiErr [LErr]
  deriving (Show, Eq)

type LResult = Either LErr

reportErr :: LErr -> String
reportErr (LErr s) = "normal error: " ++ s
reportErr (LTErr e t1 t2) = "Type error: expression " ++ pretty e ++ "\n\texpected " ++ pretty t1 ++ "\n\tbut got " ++ pretty t2
reportErr (LTFiledNotFound s) = "field " ++ s ++ " not found"
reportErr (LBug s) = "bug found: " ++ s
reportErr (LMultiErr errs) = unlines $ map reportErr errs