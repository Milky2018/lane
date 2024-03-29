module Err (LErr (..), LResult, reportErr) where 
import Ty ( pretty, LType )
import TAST (MTExpr)
import AST (LExpr)

data LErr = 
    LTErr MTExpr LType LType
  | LTFiledNotFound String
  | LBug String 
  | LMultiErr [LErr]
  | LTopLevelDefNoAnnotation String
  | LVariableNotInScope String
  | LFunctionArgumentTypeMissing String 
  | LLetrecBindingTypeMissing String
  | LConstructorNotInScope String 
  | LPatternHasWrongNumberOfArguments String [String] 
  | LBranchesHaveDifferentTypes LType LType 
  | LNoPatternMatched LExpr 
  deriving (Show, Eq)

type LResult = Either LErr

reportErr :: LErr -> String
reportErr (LTErr e t1 t2) = "Type error: expression " ++ pretty e ++ "\n\texpected " ++ pretty t1 ++ "\n\tbut got " ++ pretty t2
reportErr (LTFiledNotFound s) = "field " ++ s ++ " not found"
reportErr (LBug s) = "bug found: " ++ s
reportErr (LMultiErr errs) = unlines $ map reportErr errs
reportErr (LTopLevelDefNoAnnotation name) = "Top level expressions need type annotations: " ++ name 
reportErr (LVariableNotInScope var) = "Variable not in scope: " ++ var
reportErr (LFunctionArgumentTypeMissing arg) = "Argument is missing type annotation " ++ arg
reportErr (LLetrecBindingTypeMissing name) = "Letrec binding is missing type annotation " ++ name
reportErr (LConstructorNotInScope name) = "Constructor not in scope: " ++ name
reportErr (LPatternHasWrongNumberOfArguments cons args) = "Pattern " ++ cons ++ " has wrong number of arguments: " ++ show args
reportErr (LBranchesHaveDifferentTypes t1 t2) = "Branches have different types: " ++ pretty t1 ++ " and " ++ pretty t2
reportErr (LNoPatternMatched e) = "No pattern matched: " ++ pretty e