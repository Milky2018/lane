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
    addDef env (TLExp name ty expr) = extendEnv name (typeCheckStmt newEnv (TLExp name ty expr)) env
    newEnv = initialTEnv (Prog defs) oldEnv

typeCheckStmt :: TEnv -> MTStmt -> LType
typeCheckStmt env (TLExp _ ty expr) = case (tc expr env, ty) of
  (Right ty', Just ty'') -> if ty'' == ty' then ty' else error "typeCheckStmt: type mismatch"
  (Right ty', Nothing) -> ty' 
  (Left err, _) -> error $ "typeCheckStmt: " ++ reportErr err

typeCheck :: MTProg -> Maybe LErr
typeCheck prog@(Prog stmts) = 
  let tenv = initialTEnv prog (addTBuiltins emptyEnv) 
  in case tcStmts stmts tenv of
    Right _ -> Nothing
    Left err -> Just err

tcStmts :: [MTStmt] -> TEnv -> LResult TEnv
tcStmts ss env = foldM (flip tcStmt) env ss

tcStmt :: MTStmt -> TEnv -> LResult TEnv
tcStmt (TLExp name _ body) env = do
  t <- tc body env
  return $ extendEnv name t env

-- TODO: type inference
tc :: MTExpr -> TEnv -> LResult LType
tc (EBool _) _ = return LTBool
tc (EInt _) _ = return LTInt
tc EUnit _ = return LTUnit
tc (EString _) _ = return LTString
tc (EId x) env = case lookupEnv x env of
    Just t  -> return t
    Nothing -> Left $ LErr ("Unbound variable: " ++ x)
tc (EApp e1 e2) env = do
    t1 <- tc e1 env
    t2 <- tc e2 env
    case t1 of
        LTLam t1' t1'' | t1' == t2 -> return t1''
        _ -> Left $ LTErr (EApp e1 e2) t1 t2
tc (ELam x mt1 e mt2) env = do
    t1 <- case mt1 of
        Just t  -> return t
        Nothing -> Left $ LErr ("Missing type annotation for argument: " ++ x)
    let env' = extendEnv x t1 env
    t2 <- tc e env'
    case mt2 of
        Just t  | t == t2 -> return (LTLam t1 t2)
        Just t  -> Left $ LTErr (ELam x mt1 e mt2) t t2
        Nothing -> return (LTLam t1 t2)
tc (EIf e1 e2 e3) env = do
    t1 <- tc e1 env
    case t1 of
        LTBool -> do
            t2 <- tc e2 env
            t3 <- tc e3 env
            if t2 == t3
                then return t2
                else Left $ LTErr (EIf e1 e2 e3) t2 t3
        _ -> Left $ LTErr (EIf e1 e2 e3) t1 LTBool