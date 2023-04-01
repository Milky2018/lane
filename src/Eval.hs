{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Eval (runProg, FinalVal (..)) where

import AST
    ( LExpr, Expr(..), LProg, Prog (..), LTLStmt, TLStmt (..) )
import Err ( LResult, LErr(LBug), reportErr )
import Val ( VEnv, LVal(..) )
import Builtins ( addBuiltins )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Control.Monad (foldM)

data FinalVal =
    FinalBool Bool
  | FinalInt Int
  | FinalUnit
  | FinalString String
  | FinalErr String
  deriving (Show, Eq)

runProg :: LProg -> FinalVal 
runProg (Prog stmts) = -- add builtins
  case evalStmts stmts (addBuiltins emptyEnv) of
    Left err -> FinalErr $ reportErr err
    Right env -> case eval (EId "main") env of 
      Left err -> FinalErr $ reportErr err
      Right (LValBool b) -> FinalBool b
      Right (LValInt i) -> FinalInt i
      Right LValUnit -> FinalUnit
      Right (LValString s) -> FinalString s
      Right (LValLam _ _ _) -> FinalErr "lambda as main expression"
      Right (LValBif _) -> FinalErr "builtin function as main expression"

evalStmts :: [LTLStmt] -> VEnv -> LResult VEnv
evalStmts ss env = foldM (flip evalStmt) env ss

evalStmt :: LTLStmt -> VEnv -> LResult VEnv
evalStmt (TLExp name _ body) env = do
  v <- eval body env
  return $ extendEnv name v env

eval :: LExpr -> VEnv -> LResult LVal
eval (EBool b) env = return $ LValBool b
eval (EInt i) env = return $ LValInt i
eval EUnit env = return LValUnit
eval (EString s) env = return $ LValString s
eval (EId s) env =
  case lookupEnv s env of
    Nothing -> Left $ LBug $ "unbound variable: " ++ s
    Just v -> return v
eval (EApp e1 e2) env = do
  v1 <- eval e1 env
  case v1 of
    LValLam id' e env' -> do
      v2 <- eval e2 env
      eval e (extendEnv id' v2 env')
    LValBif bif -> do
      v2 <- eval e2 env
      bif v2
    _ -> Left (LBug (show e1 ++ "not a function"))
eval (ELam arg _ body _) env = return $ LValLam arg body env
eval (EIf cond b1 b2) env = do
  v1 <- eval cond env
  case v1 of
    LValBool b -> if b then eval b1 env else eval b2 env
    _ -> Left (LBug "not a boolean")
