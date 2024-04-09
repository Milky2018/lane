{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Eval (runProg, FinalVal (..)) where

import AST
    ( LExpr, Expr(..), LProg, Prog (..), TLStmt (..), EBranch (..) )
import Err ( LResult, LErr(LBug, LNoPatternMatched) )
import Val ( VEnv, LVal(..) )
import Builtins ( addBuiltins, trueVal, falseVal )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Control.Monad.Fix (mfix)
import Control.Monad (foldM)
import Data.List.NonEmpty (toList)

import Prettyprinter (Pretty, pretty)

data FinalVal =
    FinalVal LVal
  | FinalErr String
  deriving Eq

instance Pretty FinalVal where
  pretty (FinalVal v) = pretty v 
  pretty (FinalErr e) = pretty e 

instance Show FinalVal where 
  show = show . pretty

createInitialEnv :: LProg -> VEnv -> VEnv
createInitialEnv (Prog defs) oldEnv = foldl addDef oldEnv defs
  where
    addDef env (TLExp name _ expr) = extendEnv name (evalTopLevelExpr newEnv expr) env
    addDef env (TLEnum enum vars) = foldl addVariant env vars
      where
        addVariant :: VEnv -> (String, [()]) -> VEnv
        addVariant env' (varName, fields) = extendEnv varName (val varName fields) env'

        val :: String -> [()] -> LVal 
        val name [] = LValEnum enum name []
        val name (():xs) = LValBif $ \x -> return $ prepend x (val name xs)
        
        prepend :: LVal -> LVal -> LVal 
        prepend x (LValEnum enum' name xs) = LValEnum enum' name (x:xs)
        prepend x (LValBif f) = LValBif $ \y -> do 
          v <- f y
          return $ prepend x v
        prepend _ _ = error "impossible"

    newEnv = createInitialEnv (Prog defs) oldEnv

evalTopLevelExpr :: VEnv -> LExpr -> LVal
evalTopLevelExpr env expr = case eval expr env of
  Right value -> value
  Left err    -> error $ "evalTopLevelDef: " ++ show (pretty expr) ++ "\n" ++ show (pretty err)

-- TODO: when IO is added, the main expression should be typed IO.
runProg :: LProg -> FinalVal
runProg prog =
  let env = createInitialEnv prog (addBuiltins emptyEnv)
  in case eval (EId "main") env of
      Left err -> FinalErr $ show (pretty err)
      Right (LValLam _ _ _) -> FinalErr "lambda as main expression"
      Right (LValBif _) -> FinalErr "builtin function as main expression"
      Right v -> FinalVal v 

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
    _ -> Left (LBug (show (pretty e1) ++ "not a function"))
eval (ELam arg _ body _) env = return $ LValLam arg body env
eval (ETypeLam _ e) env = eval e env 
-- eval (EFix arg _ body _) env = mfix $ \val -> do
--   let newEnv = extendEnv arg val env
--   eval body newEnv
eval (ELetrec bindings body) env = do
  newEnv <- mfix $ \newEnv ->
        foldM
          (\env' (name, _, expr) ->
            do
            val <- eval expr newEnv
            return $ extendEnv name val env')
          env
          bindings
  eval body newEnv
eval (EIf cond b1 b2) env = do
  v1 <- eval cond env
  if v1 == trueVal then eval b1 env
  else if v1 == falseVal then eval b2 env
  else Left (LBug "not a boolean")
eval (ETypeApp e _) env = eval e env
eval e@(EMatch e0 branches) env = do
  v0 <- eval e0 env 
  case v0 of 
    LValEnum enumName variantName fields -> evalMatch variantName fields (toList branches)
    _ -> Left $ LBug "not a variant"
  where 
    evalMatch :: String -> [LVal] -> [EBranch ()] -> LResult LVal
    evalMatch var fields [] = Left $ LNoPatternMatched e
    evalMatch var fields (EBranch patCons patVars body : rest) = 
      if matchPattern patCons var 
      then eval body (foldl (\env' (arg, field) -> extendEnv arg field env') env (zip patVars fields))
      else evalMatch var fields rest

    matchPattern :: String -> String -> Bool 
    matchPattern patCons valCons = patCons == valCons || patCons == "_"
  -- v <- eval e env
  -- case v of
  --   LValEnum enumName variantName fields -> case lookupEnv variantName env of
  --     Nothing -> Left $ LBug $ "unbound variant: " ++ variantName
  --     Just (LValEnum _ _ fields') -> do
  --       let newEnv = foldl (\env' (field, field') -> extendEnv field field' env') env (zip fields fields')
  --       case lookupEnv variantName newEnv of
  --         Nothing -> Left $ LBug $ "unbound variant: " ++ variantName
  --         Just (LValBif bif) -> bif v
  --         Just (LValLam arg body env'') -> eval body newEnv
  --         _ -> Left $ LBug $ "not a function: " ++ variantName
  --     Just _ -> Left $ LBug $ "not a variant: " ++ variantName
  --   _ -> Left $ LBug "not a variant"