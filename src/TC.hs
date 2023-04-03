{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module TC (typeCheck, elimType) where
import AST ( LExpr, Expr(..), LProg, Prog (..), TLStmt (..), LTLStmt )
import Builtins ( addTBuiltins )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Err ( LResult, LErr(..), reportErr )
import TAST ( TEnv, MTProg, MTExpr, MTStmt )
import Ty ( LType(..) )
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

elimType :: MTProg -> LProg 
elimType (Prog stmts) = Prog $ map elimTypeStmt stmts 

elimTypeStmt :: MTStmt -> LTLStmt
elimTypeStmt (TLExp name _ body) = TLExp name () (elimTypeExpr body)

elimTypeExpr :: MTExpr -> LExpr
elimTypeExpr (EBool b) = EBool b
elimTypeExpr (EInt i) = EInt i
elimTypeExpr EUnit = EUnit
elimTypeExpr (EString s) = EString s
elimTypeExpr (EId s) = EId s
elimTypeExpr (EApp e1 e2) = EApp (elimTypeExpr e1) (elimTypeExpr e2)
elimTypeExpr (ELam name _ body _) = ELam name () (elimTypeExpr body) ()
elimTypeExpr (EIf e1 e2 e3) = EIf (elimTypeExpr e1) (elimTypeExpr e2) (elimTypeExpr e3) 

initialTEnv :: MTProg -> TEnv -> TEnv
initialTEnv (Prog defs) oldEnv = foldl addDef oldEnv defs
  where
    addDef env (TLExp name Nothing expr) = case tc expr Nothing env of 
        Right ty -> extendEnv name ty env 
        Left err -> error $ "Top level definitions need type annotaions" ++ reportErr err 
    addDef env (TLExp name (Just ty) _expr) = extendEnv name ty env

typeCheck :: MTProg -> Maybe LErr
typeCheck prog@(Prog stmts) = 
  let tenv = initialTEnv prog (addTBuiltins emptyEnv) 
  in case tcStmts stmts tenv of
    Right _ -> Nothing
    Left err -> Just err

tcStmts :: [MTStmt] -> TEnv -> LResult TEnv
tcStmts ss env = foldM (flip tcStmt) env ss

tcStmt :: MTStmt -> TEnv -> LResult TEnv
tcStmt (TLExp name ty body) env = do
  t <- tc body ty env
  return $ extendEnv name t env

-- TODO: type inference
tc :: MTExpr -> Maybe LType -> TEnv -> LResult LType
tc (EBool _) (Just LTBool) _ = return LTBool
tc (EBool _) (Just t) _ = Left $ LTErr (EBool True) t LTBool
tc (EBool _) Nothing _ = return LTBool
tc (EInt _) (Just LTInt) _ = return LTInt
tc (EInt n) (Just t) _ = Left $ LTErr (EInt n) t LTInt
tc (EInt _) Nothing _ = return LTInt
tc EUnit (Just LTUnit) _ = return LTUnit
tc EUnit (Just t) _ = Left $ LTErr EUnit t LTUnit
tc EUnit Nothing _ = return LTUnit
tc (EString _) (Just LTString) _ = return LTString
tc (EString s) (Just t) _ = Left $ LTErr (EString s) t LTString
tc (EString _) Nothing _ = return LTString
tc (EId x) t env = case lookupEnv x env of
    Just t' -> case t of
        Just t'' | t'' == t' -> return t'
        Just t'' -> Left $ LTErr (EId x) t'' t'
        Nothing -> return t'
    Nothing -> Left $ LErr ("Unbound variable: " ++ x)
tc (EApp e1 e2) t env = do
    t1 <- tc e1 Nothing env
    t2 <- tc e2 Nothing env
    checkedType <- case t1 of
            LTLam t1' t1'' -> if t1' == t2 then return t1'' else Left $ LTErr e1 t1' t2
            _ -> Left $ LTErr e1 (LTLam t2 (LTVar 0)) t1
    case t of 
        Just t' | t' == checkedType -> return t'
        Just t' -> Left $ LTErr (EApp e1 e2) t' checkedType
        Nothing -> return checkedType
tc (ELam x mt1 e mt2) Nothing env = do
    t1 <- case mt1 of
        Just t  -> return t
        Nothing -> Left $ LErr ("Missing type annotation for argument: " ++ x)
    let env' = extendEnv x t1 env
    t2 <- tc e mt2 env'
    return (LTLam t1 t2)
tc (ELam x mt1 e mt2) (Just (LTLam t1 t2)) env = do
    let eShouldType = tc e (Just t2) (extendEnv x t1 env)
    _ <- case (mt1, mt2) of 
        (Just t1', Just t2') | t1 == t1' && t2 == t2' -> eShouldType
        (Just t1', Nothing) | t1 == t1' -> eShouldType
        (Nothing, Just t2') | t2 == t2' -> eShouldType
        (Nothing, Nothing) -> eShouldType
        _ -> Left $ LTErr (ELam x mt1 e mt2) (LTLam t1 t2) (LTLam (fromMaybe t1 mt1) (fromMaybe t2 mt2))
    return (LTLam t1 t2)
tc (ELam x mt1 e mt2) (Just t) _ = Left $ LTErr (ELam x mt1 e mt2) t (LTLam (fromMaybe (LTVar 0) mt1) (fromMaybe (LTVar 0) mt2))
tc (EIf e1 e2 e3) t env = do
    _t1 <- tc e1 (Just LTBool) env
    t2 <- tc e2 t env
    t3 <- tc e3 t env
    if t2 == t3 then return t2 else Left $ LTErr (EIf e1 e2 e3) t2 t3