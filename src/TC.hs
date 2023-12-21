{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Eta reduce" #-}
module TC (typeCheck, elimTypeProg) where
import AST ( Expr(..), LProg, Prog (..), TLStmt (..), transProg, EBranch (..))
import Builtins ( addTBuiltins, stringType, boolType, intType )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Err ( LResult, LErr(..) )
import TAST ( MTProg, TEnv, MTStmt, MTExpr )
import Ty ( LType (..), LType, UDT )
import Data.Maybe (fromMaybe, fromJust)
import Builtintypes (addBuiltinTypes)
import Udt (initialTEnv)
import Control.Monad (foldM, foldM_)

elimType :: Maybe LType -> ()
elimType _ = ()

elimTypeProg :: MTProg -> LProg
elimTypeProg = transProg elimType

-- Typecheck a program, and maybe returns an error if the program does not 
-- typecheck or has some other errors. 
typeCheck :: MTProg -> Maybe LErr
typeCheck prog =
  case initialTEnv prog (addTBuiltins emptyEnv) (addBuiltinTypes []) of
    Right (tenv, udt) -> tcProg prog tenv udt
    Left err -> Just err

tcProg :: MTProg -> TEnv -> UDT -> Maybe LErr
tcProg (Prog stmts) env udt = tcStmts stmts env udt

tcStmts :: [MTStmt] -> TEnv -> UDT -> Maybe LErr
tcStmts ss env udt = let errors = map (\stmt -> tcStmt stmt env udt) ss in
  case filter (/= Nothing) errors of
    [] -> Nothing
    errs -> Just $ head $ map fromJust errs

tcStmt :: MTStmt -> TEnv -> UDT -> Maybe LErr
tcStmt (TLExp _name ty body) env udt =
  case tc body ty env udt of
    Right _ -> Nothing
    Left err -> Just err
tcStmt (TLEnum _name _variants) _env _udt = Nothing

-- This type checker uses a simple bidirectional algorithm
-- tc :: expr -> expected type -> type env -> udt -> result 
tc :: MTExpr -> Maybe LType -> TEnv -> UDT -> LResult LType
tc (EInt _) (Just t) _ _ | t == intType = return intType
tc (EInt n) (Just t) _ _ = Left $ LTErr (EInt n) t intType
tc (EInt _) Nothing _ _ = return intType

tc (EString _) (Just t) _ _ | t == stringType = return stringType
tc (EString s) (Just t) _ _ = Left $ LTErr (EString s) t stringType
tc (EString _) Nothing _ _ = return stringType

tc (EId x) t env _udt = case lookupEnv x env of
  Just t' -> case t of
    Just t'' | t'' == t' -> return t'
    Just t'' -> Left $ LTErr (EId x) t'' t'
    Nothing -> return t'
  Nothing -> Left $ LVariableNotInScope x 

tc (EApp e1 e2) t env udt = do
  t2 <- tc e2 Nothing env udt
  case t of
    Just t' -> do
      t1 <- tc e1 (Just (LTLam t2 t')) env udt
      if t1 == LTLam t2 t' then return t' else Left $ LBug "Application expression's type is not arrow"
    Nothing -> do
      t1 <- tc e1 Nothing env udt
      case t1 of
        LTLam t1' t1'' | t1' == t2 -> return t1''
        LTLam t1' _t1'' -> Left $ LTErr e2 t1' t2
        t'' -> Left $ LTErr (EApp e1 e2) t2 t''

tc (ELam x mt1 e mt2) Nothing env udt = do
  t1 <- case mt1 of
    Just t  -> return t
    Nothing -> Left $ LFunctionArgumentTypeMissing x 
  let env' = extendEnv x t1 env
  t2 <- tc e mt2 env' udt
  return (LTLam t1 t2)
tc (ELam x mt1 e mt2) (Just (LTLam t1 t2)) env udt = do
  let eShouldType = tc e (Just t2) (extendEnv x t1 env) udt
  _ <- case (mt1, mt2) of
    (Just t1', Just t2') | t1 == t1' && t2 == t2' -> eShouldType
    (Just t1', Nothing) | t1 == t1' -> eShouldType
    (Nothing, Just t2') | t2 == t2' -> eShouldType
    (Nothing, Nothing) -> eShouldType
    _ -> Left $ LTErr (ELam x mt1 e mt2) (LTLam t1 t2) (LTLam (fromMaybe t1 mt1) (fromMaybe t2 mt2))
  return (LTLam t1 t2)
tc (ELam _x _mt1 _e _mt2) (Just t) _ _ = Left $
  LTErr (ELam _x _mt1 _e _mt2) t (LTLam (fromMaybe t _mt1) (fromMaybe t _mt2))

tc (ELetrec bindings e) should env udt = do
  newEnv <- foldM
      (\env' (name, mt, _expr) -> case mt of
        Just t -> return $ extendEnv name t env'
        Nothing -> Left $ LLetrecBindingTypeMissing name)
      env
      bindings
  foldM_
    (\env' (name, mt, expr) ->
      do
      t <- tc expr mt newEnv udt
      return $ extendEnv name t env')
    env
    bindings
  tc e should newEnv udt

tc (EIf e1 e2 e3) t env udt = do
  _t1 <- tc e1 (Just boolType) env udt
  t2 <- tc e2 t env udt
  t3 <- tc e3 t env udt
  if t2 == t3 then return t2 else Left $ LTErr (EIf e1 e2 e3) t2 t3

tc (EMatch e0 branches) t env udt = do
  t0 <- tc e0 Nothing env udt
  branchTypes <- mapM (tcBranch t t0 env udt) branches
  foldM
    (\t1 t2 -> if t1 == t2 then return t1 else Left $ LBranchesHaveDifferentTypes t1 t2)
    (head branchTypes)
    branchTypes

-- tcBranch (target type, e0 type, env, udt, branch)
tcBranch :: Maybe LType -> LType -> TEnv -> UDT -> EBranch (Maybe LType) -> LResult LType
tcBranch t t0 env udt (EBranch cons args body) = do
  consType <- case lookupEnv cons env of
    Just ty -> return ty
    Nothing -> Left $ LConstructorNotInScope cons
  env' <- case addBinding consType args env of 
    Just e -> return e 
    Nothing -> Left $ LPatternHasWrongNumberOfArguments cons args 
  tc body t env' udt
  where 
    addBinding :: LType -> [String] -> TEnv -> Maybe TEnv 
    addBinding ty [] env' | ty == t0 = return env'
    addBinding (LTLam t1 t2) (arg:args') env' = addBinding t2 args' (extendEnv arg t1 env')
    addBinding _ _ _ = Nothing 