{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Eval where
import AST
import Err
import Val 
import Builtins
import Env

data FinalVal = 
    FinalBool Bool 
  | FinalInt Int 
  | FinalUnit
  | FinalString String
  | FinalErr String
  deriving (Show, Eq)

runProg :: LProg -> FinalVal
runProg (LProg e) = -- add builtins
  case eval e (addBuiltins emptyEnv) of
    Left err -> FinalErr $ reportErr err
    Right (LValBool b) -> FinalBool b
    Right (LValInt i) -> FinalInt i
    Right LValUnit -> FinalUnit
    Right (LValString s) -> FinalString s
    Right (LValLam _ _ _) -> FinalErr "lambda in top-level expression"
    Right (LValBif _) -> FinalErr "builtin function"

eval :: LExpr -> VEnv -> LResult LVal
eval (LExprBool b) env = return $ LValBool b
eval (LExprInt i) env = return $ LValInt i
eval LExprUnit env = return LValUnit
eval (LExprString s) env = return $ LValString s
eval (LExprId s) env = 
  case envLookup s env of 
    Nothing -> Left $ LBug $ "unbound variable: " ++ s
    Just v -> return v
eval (LExprApp e1 e2) env = do
  v1 <- eval e1 env
  case v1 of 
    LValLam id e env' -> do
      v2 <- eval e2 env
      eval e (extendEnv id v2 env')
    LValBif bif -> do 
      v2 <- eval e2 env
      bif v2
    _ -> Left (LBug (show e1 ++ "not a function"))
eval (LExprLam arg body) env = return $ LValLam arg body env
eval (LExprIf cond b1 b2) env = do 
  v1 <- eval cond env 
  case v1 of 
    LValBool b -> if b then eval b1 env else eval b2 env
    _ -> Left (LBug "not a boolean")
