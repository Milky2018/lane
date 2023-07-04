{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Eta reduce" #-}
module TC (typeCheck, elimType) where
import AST ( LExpr, Expr(..), LProg, Prog (..), TLStmt (..), LTLStmt )
import Builtins ( addTBuiltins )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Err ( LResult, LErr(..) )
import TAST ( MTProg, MTExpr, MTStmt, TVEnv, TVProg, TVStmt, TVExpr, lookallup )
import Ty ( LTypeVal (..) )
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Bifunctor
import Builtintypes (addBuiltinTypes)
import Udt (initialTEnv)
import Pretty (pretty)
-- import qualified Data.Bifunctor

elimType :: MTProg -> LProg
elimType (Prog stmts) = Prog $ map elimTypeStmt stmts

elimTypeStmt :: MTStmt -> LTLStmt
elimTypeStmt (TLExp name _ body) = TLExp name () (elimTypeExpr body)
-- TODO: can be filtered
elimTypeStmt (TLStruct struct fields) = TLStruct struct elimedFields
  where
    elimedFields = map (\(name, _ty) -> (name, ())) fields

elimTypeExpr :: MTExpr -> LExpr
elimTypeExpr (EInt i) = EInt i
elimTypeExpr (EString s) = EString s
elimTypeExpr (EId s) = EId s
elimTypeExpr (EApp e1 e2) = EApp (elimTypeExpr e1) (elimTypeExpr e2)
elimTypeExpr (ELam name _ body _) = ELam name () (elimTypeExpr body) ()
elimTypeExpr (EFix name _ body _) = EFix name () (elimTypeExpr body) () 
elimTypeExpr (EIf e1 e2 e3) = EIf (elimTypeExpr e1) (elimTypeExpr e2) (elimTypeExpr e3)
elimTypeExpr (EAccess e field) = EAccess (elimTypeExpr e) field
elimTypeExpr (EStruct name fields) = EStruct name (map (Data.Bifunctor.second elimTypeExpr) fields)

typeCheck :: MTProg -> Maybe LErr
typeCheck prog =
  case initialTEnv prog (addTBuiltins emptyEnv) (addBuiltinTypes emptyEnv) of
    Right (tenv, udt) -> tcProg (lookallup udt prog) tenv
    Left err -> Just err

tcProg :: TVProg -> TVEnv -> Maybe LErr
tcProg (Prog stmts) env = tcStmts stmts env

tcStmts :: [TVStmt] -> TVEnv -> Maybe LErr
tcStmts ss env = let errors = map (`tcStmt` env) ss in
  case filter (/= Nothing) errors of
    [] -> Nothing
    errs -> Just $ head $ map fromJust errs

tcStmt :: TVStmt -> TVEnv -> Maybe LErr
tcStmt (TLExp _name ty body) env =
  case tc body ty env of
    Right _ -> Nothing
    Left err -> Just err
tcStmt (TLStruct _struct _fields) _env = Nothing

-- This type checker uses a simple bidirectional algorithm
-- tc :: expr -> expected type -> type env -> udt -> result 
tc :: TVExpr -> Maybe LTypeVal -> TVEnv -> LResult LTypeVal
tc (EInt _) (Just TVInt) _ = return TVInt
tc (EInt n) (Just t) _ = Left $ LTErr (EInt n) t TVInt
tc (EInt _) Nothing _ = return TVInt

tc (EString _) (Just TVString) _ = return TVString
tc (EString s) (Just t) _ = Left $ LTErr (EString s) t TVString
tc (EString _) Nothing _ = return TVString

tc (EId x) t env = case lookupEnv x env of
  Just t' -> case t of
    Just t'' | t'' == t' -> return t'
    Just t'' -> Left $ LTErr (EId x) t'' t'
    Nothing -> return t'
  Nothing -> Left $ LErr ("Unbound variable: " ++ x)

tc (EApp e1 e2) t env = do
  t2 <- tc e2 Nothing env 
  case t of
    Just t' -> do
      t1 <- tc e1 (Just (TVLam t2 t')) env 
      if t1 == TVLam t2 t' then return t' else Left $ LBug "This should never happend"
    Nothing -> do
      t1 <- tc e1 Nothing env 
      case t1 of
        TVLam t1' t1'' | t1' == t2 -> return t1''
        TVLam t1' _t1'' -> Left $ LTErr e2 t1' t2
        t'' -> Left $ LTErr (EApp e1 e2) t2 t''

tc (ELam x mt1 e mt2) Nothing env = do
  t1 <- case mt1 of
    Just t  -> return t
    Nothing -> Left $ LErr ("Missing type annotation for argument: " ++ x)
  let env' = extendEnv x t1 env
  t2 <- tc e mt2 env' 
  return (TVLam t1 t2)
tc (ELam x mt1 e mt2) (Just (TVLam t1 t2)) env = do
  let eShouldType = tc e (Just t2) (extendEnv x t1 env)
  _ <- case (mt1, mt2) of
    (Just t1', Just t2') | t1 == t1' && t2 == t2' -> eShouldType
    (Just t1', Nothing) | t1 == t1' -> eShouldType
    (Nothing, Just t2') | t2 == t2' -> eShouldType
    (Nothing, Nothing) -> eShouldType
    _ -> Left $ LTErr (ELam x mt1 e mt2) (TVLam t1 t2) (TVLam (fromMaybe t1 mt1) (fromMaybe t2 mt2))
  return (TVLam t1 t2)
tc (ELam _x _mt1 _e _mt2) (Just t) _ = Left $
  LErr $ "Expect a function type, but got" ++ show t

tc fix@(EFix f mt1 e mt2) Nothing env = 
  case (mt1, mt2) of 
    (Just t1, Just t2) | t1 == t2 -> return t1
    (Just t1, Just t2) -> Left $ LTErr (EFix f mt1 e mt2) t1 t2
    (Just t1, Nothing) -> do 
      t2 <- tc e (Just t1) (extendEnv f t1 env)
      if t1 == t2 then return t1 else Left $ LTErr (EFix f mt1 e mt2) t1 t2
    (Nothing, Just t2) -> return t2
    (Nothing, Nothing) -> Left $ LErr $ "Missing type annotation for argument: " ++ f ++ " in " ++ pretty fix
tc (EFix f mt1 e mt2) (Just t) env =
  case (mt1, mt2) of 
    (Just t1, Just t2) | t1 == t2 && t == t1 -> return t1
    (Just t1, Just t2) | t1 == t2 -> Left $ LTErr (EFix f mt1 e mt2) t t1
    (Just t1, Just t2) -> Left $ LTErr (EFix f mt1 e mt2) t1 t2
    (Just t1, Nothing) | t == t1 -> do 
      t2 <- tc e (Just t1) (extendEnv f t1 env)
      if t1 == t2 then return t1 else Left $ LTErr (EFix f mt1 e mt2) t1 t2
    (Just t1, Nothing) -> Left $ LTErr (EFix f mt1 e mt2) t t1
    (Nothing, Just t2) | t == t2 -> return t2
    (Nothing, Just t2) -> Left $ LTErr (EFix f mt1 e mt2) t t2
    (Nothing, Nothing) -> do 
      t2 <- tc e (Just t) (extendEnv f t env)
      if t == t2 then return t else Left $ LTErr (EFix f mt1 e mt2) t t2

tc (EIf e1 e2 e3) t env = do
  _t1 <- tc e1 (Just TVBool) env 
  t2 <- tc e2 t env 
  t3 <- tc e3 t env 
  if t2 == t3 then return t2 else Left $ LTErr (EIf e1 e2 e3) t2 t3

tc (EStruct n fields) Nothing env = do 
  fieldTypes <- mapM fieldType fields
  return $ TVStruct n fieldTypes where 
    fieldType (field, expr) = do
      t <- tc expr Nothing env
      return (field, t)

tc (EStruct n fields) (Just should) env =
  case should of 
    TVStruct n' fieldTypes -> do
      if length fields /= length fieldTypes then Left $ LTErr (EStruct n fields) should (TVStruct n fieldTypes) 
      else do
        fieldTypes' <- mapM fieldType fields
        if n == n' && fieldTypes == fieldTypes' then return should else Left $ LTErr (EStruct n fields) should (TVStruct n fieldTypes')
      where 
        fieldType (field, expr) = 
          case lookup field fieldTypes of 
            Just expect -> do
              t <- tc expr (Just expect) env
              return (field, t)
            Nothing -> Left $ LTFiledNotFound field
    _ -> Left $ LTErr (EStruct n fields) should (TVStruct n [])

tc (EAccess expr field) should env = do
  t <- tc expr Nothing env
  case t of 
    TVStruct _name fields -> case lookup field fields of 
      Just t' -> case should of 
        Just t'' | t'' == t' -> return t'
        Just t'' -> Left $ LTErr (EAccess expr field) t'' t'
        Nothing -> return t'
      Nothing -> Left $ LTFiledNotFound field
    _ -> Left $ LTErr (EAccess expr field) (TVStruct "<struct>" []) t