{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Eval (runProg, FinalVal (..)) where

import AST
    ( LExpr, Expr(..), LProg, Prog (..), TLStmt (..) )
import Err ( LResult, LErr(LBug), reportErr )
import Val ( VEnv, LVal(..) )
import Builtins ( addBuiltins )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Pretty (pretty)
import Control.Monad.Except (mfix)

data FinalVal =
    FinalBool Bool
  | FinalInt Int
  | FinalUnit
  | FinalString String
  | FinalErr String
  | FinalStruct String [(String, LVal)]
  deriving (Show, Eq)

createInitialEnv :: LProg -> VEnv -> VEnv
createInitialEnv (Prog defs) oldEnv = foldl addDef oldEnv defs
  where
    addDef env stmt@(TLExp name _ expr) = extendEnv name (evalTopLevelExpr newEnv expr) env
    addDef env stmt@(TLStruct _ _) = env 
    newEnv = createInitialEnv (Prog defs) oldEnv
    
evalTopLevelExpr :: VEnv -> LExpr -> LVal
evalTopLevelExpr env expr = case eval expr env of
  Right value -> value
  Left err    -> error $ "evalTopLevelDef: " ++ pretty expr ++ "\n" ++ reportErr err

-- TODO: when IO is added, the main expression should be typed IO.
runProg :: LProg -> FinalVal
runProg prog = 
  let env = createInitialEnv prog (addBuiltins emptyEnv)
  in case eval (EId "main") env of
      Left err -> FinalErr $ reportErr err
      Right (LValBool b) -> FinalBool b
      Right (LValInt i) -> FinalInt i
      Right LValUnit -> FinalUnit
      Right (LValString s) -> FinalString s
      Right (LValStruct s fields) -> FinalStruct s fields
      Right (LValLam _ _ _) -> FinalErr "lambda as main expression"
      Right (LValBif _) -> FinalErr "builtin function as main expression"

eval :: LExpr -> VEnv -> LResult LVal
eval (EInt i) env = return $ LValInt i
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
eval (EFix arg _ body _) env = mfix $ \val -> do
  let newEnv = extendEnv arg val env
  eval body newEnv
eval (EIf cond b1 b2) env = do
  v1 <- eval cond env
  case v1 of
    LValBool b -> if b then eval b1 env else eval b2 env
    _ -> Left (LBug "not a boolean")
eval (EStruct name fields) env = do
  fields' <- mapM (\(id', expr) -> do
    v <- eval expr env
    return (id', v)) fields
  return $ LValStruct name fields'
eval (EAccess e field) env = 
  case eval e env of 
    Right (LValStruct _ fields) -> case lookup field fields of 
      Just v -> return v 
      Nothing -> Left $ LBug $ "field " ++ field ++ " not found"
    Right _ -> Left $ LBug "not a struct"
    Left err -> Left err