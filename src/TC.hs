{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Eta reduce" #-}
module TC (typeCheck, elimTypeProg) where
import AST ( Expr(..), LProg, Prog (..), TLStmt (..), transProg, EBranch (..))
import Builtins ( addTBuiltins, stringType, boolType, intType )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Err ( LResult, LErr(..) )
import TAST ( MTProg, TEnv, MTStmt, MTExpr )
import Ty ( LCon (..), LCon, Univ, typeArr, typeForall, subst, tappOnType )
import Data.Maybe (fromMaybe, fromJust)
import Data.List.NonEmpty (NonEmpty(..), map, head)
import Builtintypes (addBuiltinTypes)
import Udt (initialTEnv)
import Control.Monad (foldM, foldM_)

elimType :: Maybe LCon -> ()
elimType _ = ()

elimTypeProg :: MTProg -> LProg
elimTypeProg = transProg elimType

-- Typecheck a program, and maybe returns an error if the program does not 
-- typecheck or has some other errors. 
typeCheck :: MTProg -> Maybe LErr
typeCheck prog =
  case initialTEnv prog (addTBuiltins emptyEnv) (addBuiltinTypes emptyEnv) of
    Right (tenv, udt) -> tcProg prog tenv udt
    Left err -> Just err

tcProg :: MTProg -> TEnv -> Univ -> Maybe LErr
tcProg (Prog stmts) env udt = tcStmts stmts env udt

tcStmts :: [MTStmt] -> TEnv -> Univ -> Maybe LErr
tcStmts ss env udt = let errors = Prelude.map (\stmt -> tcStmt stmt env udt) ss in
  case filter (/= Nothing) errors of
    [] -> Nothing
    (err:errs) -> Just $ LMultiErr (Data.List.NonEmpty.map fromJust (err :| errs))

tcStmt :: MTStmt -> TEnv -> Univ -> Maybe LErr
tcStmt (TLExp _name ty body) env udt =
  case tc body ty env udt of
    Right _ -> Nothing
    Left err -> Just err
tcStmt (TLEnum _name _targs _variants) _env _udt = Nothing

-- This type checker uses a simple bidirectional algorithm
-- tc :: expr -> expected type -> type env -> udt -> result 
tc :: MTExpr -> Maybe LCon -> TEnv -> Univ -> LResult LCon
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
      t1 <- tc e1 (Just (typeArr t2 t')) env udt
      if t1 == typeArr t2 t' then return t' else Left $ LBug "Application expression's type is not arrow"
    Nothing -> do
      t1 <- tc e1 Nothing env udt
      case t1 of
        LTApp (LTApp LTArr t1') t1'' | t1' == t2 -> return t1''
        LTApp (LTApp LTArr t1') _ -> Left $ LTErr e2 t1' t2
        t'' -> Left $ LTErr (EApp e1 e2) t2 t''

tc (ELam x mt1 e mt2) Nothing env udt = do
  t1 <- case mt1 of
    Just t  -> return t
    Nothing -> Left $ LFunctionArgumentTypeMissing x
  let env' = extendEnv x t1 env
  t2 <- tc e mt2 env' udt
  return (typeArr t1 t2)
tc (ELam x mt1 e mt2) (Just (LTApp (LTApp LTArr t1) t2)) env udt = do
  let eShouldType = tc e (Just t2) (extendEnv x t1 env) udt
  _ <- case (mt1, mt2) of
    (Just t1', Just t2') | t1 == t1' && t2 == t2' -> eShouldType
    (Just t1', Nothing) | t1 == t1' -> eShouldType
    (Nothing, Just t2') | t2 == t2' -> eShouldType
    (Nothing, Nothing) -> eShouldType
    _ -> Left $ LTErr (ELam x mt1 e mt2) (typeArr t1 t2) (typeArr (fromMaybe t1 mt1) (fromMaybe t2 mt2))
  return (typeArr t1 t2)
tc (ELam _x _mt1 _e _mt2) (Just t) _ _ = Left $
  LTErr (ELam _x _mt1 _e _mt2) t (typeArr (fromMaybe t _mt1) (fromMaybe t _mt2))

tc (ETypeLam x e) Nothing env udt = do
  t <- tc e Nothing env udt
  return $ typeForall x t
tc (ETypeLam x e) (Just (LTAll x' _k t)) env udt = do
  t' <- tc e Nothing (extendEnv x (LTId x') env) udt
  if t == t' then return (typeForall x t) else Left $ LTErr (ETypeLam x e) (typeForall x t) (typeForall x t')
tc (ETypeLam _x _e) (Just t) _ _ = Left $ LTErr (ETypeLam _x _e) t (typeForall _x t)

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
    (Data.List.NonEmpty.head branchTypes)
    branchTypes

tc (ETypeApp e t) should env udt = do
  t' <- case t of 
    Just t'' -> return t''
    Nothing -> Left $ LBug "Type application without type"
  t'' <- tc e Nothing env udt
  res <- case t'' of 
    LTAll arg _k ty -> return $ subst arg t' ty
    _ -> Left $ LTypeAppOnNonForall t''
  case should of
    Just should' | should' == res -> return res
    Just should' -> Left $ LTErr (ETypeApp e t) should' res
    Nothing -> return res

-- tcBranch (target type, e0 type, env, udt, branch)
tcBranch :: Maybe LCon -> LCon -> TEnv -> Univ -> EBranch (Maybe LCon) -> LResult LCon
tcBranch t t0 env udt (EBranch cons args body) =
  if cons == "_" && null args
  then tc body t env udt
  else do
    consType <- case lookupEnv cons env of
      Just ty -> return ty
      Nothing -> Left $ LConstructorNotInScope cons
    -- extract type arguments of t0 
    let t0Args = extractTypeArgs t0
    -- apply type arguments to consType 
    let consType' = applyTypeArgs consType (reverse t0Args)
    env' <- case addBinding consType' args env of
      Just e -> return e
      Nothing -> Left $ LPatternHasWrongNumberOfArguments cons args 
    tc body t env' udt
  where
    extractTypeArgs :: LCon -> [LCon]
    extractTypeArgs (LTApp t1 t2) = t2 : extractTypeArgs t1
    extractTypeArgs _ = []

    applyTypeArgs :: LCon -> [LCon] -> LCon
    applyTypeArgs t' targs = foldl tappOnType t' targs

    addBinding :: LCon -> [String] -> TEnv -> Maybe TEnv
    addBinding ty [] env' | ty == t0 = return env'
    addBinding (LTApp (LTApp LTArr t1) t2) (arg:args') env' = extendEnv arg t1 <$> addBinding t2 args' env'
    addBinding _ _ _ = Nothing
