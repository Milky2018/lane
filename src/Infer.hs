module Infer (typeCheck, elimTypeProg) where 
import Control.Monad.State
import Ty (LCon (..), subst, Univ, LKind (..), typeArr, conApp)
import TAST
import AST
import Err
import Udt
import Builtins
import Env
import Builtintypes
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty(..), map)
import Kind (calcKind)
import Control.Monad (foldM)
import qualified Data.List.NonEmpty as NonEmpty
import Prettyprinter

type Subst = [(String, LCon)]
type Infer a = StateT Int LResult a
-- type Unifier = (Subst, [TEquation])
type TEquation = (LCon, LCon)

unify :: [TEquation] -> Maybe Subst
-- unify = undefined
unify [] = Just []
unify ((t1, t2) : cs)
  | t1 == t2 = unify cs
unify ((LCVar v, t) : cs) = bind v t cs
unify ((t, LCVar v) : cs) = bind v t cs
unify ((LCAll u1 k1 c1, LCAll u2 k2 c2) : cs) = 
  if k1 == k2 
    then unify ((c1, subst u2 (LCId u1) c2) : cs)
    else Nothing
unify ((LCApp c1 c2, LCApp c3 c4) : cs) = unify ((c1, c3) : (c2, c4) : cs)
unify eqs = error $ "Unification failed. Equations: " ++ show (pretty eqs)

bind :: String -> LCon -> [TEquation] -> Maybe Subst 
bind v t cs
  | t == LCVar v = unify cs
  | occurs v t = Nothing
  | otherwise = do
    s <- unify (applySubst (v, t) cs)
    Just ((v, t) : s)

freshTVar :: Infer LCon
freshTVar = do
  n <- get
  put (n + 1)
  return (LCVar ("$tv" ++ show n))

-- only checks for typevar
occurs :: String -> LCon -> Bool
occurs _v LCArr = False
occurs _v (LCId _id) = False  
occurs v (LCAll s _k c) = if v == s then False else occurs v c
occurs v (LCVar v') = v == v'
occurs v (LCApp c1 c2) = occurs v c1 || occurs v c2 
occurs v (LCLam u c) = if v == u then False else occurs v c

applySubst :: (String, LCon) -> [TEquation] -> [TEquation]
applySubst (v, t) = Prelude.map (\(t1, t2) -> (subst v t t1, subst v t t2))

applySubstToType :: Subst -> LCon -> LCon
applySubstToType s t = foldl (\t' (v, t'') -> subst v t'' t') t s

elimType :: Maybe LCon -> ()
elimType _ = ()

elimTypeProg :: MTProg -> LProg
elimTypeProg = transProg elimType

typeCheck :: MTProg -> Maybe LErr
typeCheck prog =
  case initialTEnv prog (addTBuiltins emptyEnv) (addBuiltinTypes emptyEnv) of
    Right (tenv, univ) -> runInfer prog tenv univ
    Left err -> Just err

infer :: MTExpr -> TEnv -> Infer (LCon, [TEquation])
-- I |- n : int -| {}
infer (EInt _) _ = return (intType, [])
-- I |- s : string -| {}
infer (EString _) _ = return (stringType, [])
-- I, x : t |- x : t -| {}
infer (EId x) env = case lookupEnv x env of
  Just t -> return (t, [])
  Nothing -> lift $ Left $ LVariableNotInScope x
-- I |- e1 : t1 -| C1
-- I |- e2 : t2 -| C2
-- I |- t fresh 
-- --- 
-- I |- e1 e2 : t -| C1; C2; t1 = t2 -> t 
infer (EApp e1 e2) env = do
  (t2, cs2) <- infer e2 env 
  (t1, cs1) <- infer e1 env 
  t <- freshTVar
  return (t, (t1, typeArr t2 t) : cs1 ++ cs2)
-- I, x : t1 |- e : t2' -| C 
-- --- 
-- I |- (\x : t1. e : t2) : t1 -> t2' -| C; t2 = t2' 
infer (ELam x (Just t1) e (Just t2)) env = do
  (t2', cs) <- infer e (extendEnv x t1 env) 
  return (typeArr t1 t2', (t2, t2') : cs)
-- I |- u fresh
-- I, x : u |- e : t2' -| C
-- ---
-- I |- (\x.e : t2) : u -> t2' -| C; t2 = t2'
infer (ELam x Nothing e (Just t2)) env = do 
  u <- freshTVar
  (t2', cs) <- infer e (extendEnv x u env)
  return (typeArr u t2', (t2, t2') : cs)
-- I, x : t1 |- e : t2' -| C 
-- --- 
-- I |- (\x : t1. e) : t1 -> t2' -| C 
infer (ELam x (Just t1) e Nothing) env = do 
  (t2', cs) <- infer e (extendEnv x t1 env)
  return (typeArr t1 t2', cs)
-- I |- u fresh 
-- I, x : u |- e : t2' -| C
-- --- 
-- I |- \x.e : u -> t2' -| C
infer (ELam x Nothing e Nothing) env = do 
  u <- freshTVar
  (t2', cs) <- infer e (extendEnv x u env)
  return (typeArr u t2', cs)
-- I |- e : t -| C 
-- --- 
-- I |- \A. e : <A> t -| C 
infer (ETypeLam u e) env = do 
  (t, cs) <- infer e env
  return (LCAll u LKType t, cs) 
-- I |- e : <A> t -| C 
-- --- 
-- I |- e[t'] : t[t'/A] -| C 
infer (ETypeApp e (Just t)) env = do 
  (t', cs) <- infer e env 
  case t' of 
    LCAll u _k t'' -> return (subst u t t'', cs)
    _ -> lift $ Left $ LTypeAppOnNonForall t' 
infer (ETypeApp _e Nothing) _env = lift (Left (LBug "Type application without type"))
-- I, x1 : t1, x2 : t2 |- e1 : t1' -| C1 
-- I, x1 : t1, x2 : t2 |- e2 : t2' -| C2 
-- I, x1 : t1, x2 : t2 |- e : t -| C 
-- --- 
-- I |- letrec x1 : t1 = e1, x2 : t2 = e2 in e : t -| C; C1; C2; t1 = t1'; t2 = t2' 
infer (ELetrec bindings e) env = do 
  let newEnv = foldl (\env' (name, mt, _expr) -> case mt of
        Just t -> extendEnv name t env'
        Nothing -> env')
        env
        bindings
  -- get t1', t2', ... tn' and C1, C2, ... Cn 
  (tys, css) <- foldM
    (\(tys', css') (_name, _mt, expr) -> do 
      (t, cs) <- infer expr newEnv 
      return (t : tys', cs : css'))
    ([], [])
    bindings
  (t, cs) <- infer e newEnv 
  let eqs = concat $ zipWith (\(_name, mt, _expr) t'' -> case mt of 
        Just t' -> [(t'', t')]
        Nothing -> []) bindings tys
  return (t, concat css ++ cs ++ eqs) 
-- I |- e1 : t1 -| C1
-- I |- e2 : t2 -| C2
-- I |- e3 : t3 -| C3
-- I |- t fresh 
-- ---
-- I |- if e1 then e2 else e3 : t -| C1; C2; C3; t1 = Bool; t = t2; t = t3
infer (EIf e1 e2 e3) env = do 
  (t1, cs1) <- infer e1 env 
  (t2, cs2) <- infer e2 env 
  (t3, cs3) <- infer e3 env 
  t <- freshTVar
  return (t, (t1, boolType) : (t, t2) : (t, t3) : cs1 ++ cs2 ++ cs3)
-- I, ||pat1|| |- e1 : t1 -| C1 
-- I, ||pat2|| |- e2 : t2 -| C2 
-- I |- e : t -| C
-- I |- u fresh
-- --- 
-- I |- match e { pat1 => e1, pat2 => e2 } : u -| C1; C2; C; u = t1; u = t2 
infer (EMatch e branches) env = do 
  u <- freshTVar
  (t, cs) <- infer e env
  tysEqs <- mapM (inferBranch t env) branches
  let (ts, eqs) = unzip (NonEmpty.toList tysEqs)
  -- u = t1; u = t2 
  let ueqs = Prelude.map (\t' -> (u, t')) ts
  return (u, ueqs ++ concat eqs ++ cs)
  where
    -- I, ||pat||, originalT |- e : t -| C; t = originalT 
    inferBranch :: LCon -> TEnv -> EBranch (Maybe LCon) -> Infer (LCon, [TEquation])
    inferBranch originalT envir (EBranch patCons patVars body) = 
      extendWithPattern patCons patVars originalT envir >>= infer body 

    -- constructor, arguments, original type, original environment
    extendWithPattern :: String -> [String] -> LCon -> TEnv -> Infer TEnv
    -- I, [_] => I 
    extendWithPattern "_" _ _ env' = return env'
    -- cons : <a> <b> a -> b -> t0 a b 
    -- I, [cons x1 x2] => I[x1 : a, x2 : b]
    extendWithPattern cons args t env' = do 
      let tArgs = extractTypeArgs t
      consType <- case lookupEnv cons env' of 
        Just ty -> return (applyTypeArgs ty (reverse tArgs))
        Nothing -> lift $ Left $ LConstructorNotInScope cons
      addBinding t consType args env' 

    extractTypeArgs :: LCon -> [LCon]
    extractTypeArgs (LCApp t1 t2) = t2 : extractTypeArgs t1
    extractTypeArgs _ = []

    applyTypeArgs :: LCon -> [LCon] -> LCon
    applyTypeArgs t' targs = foldl conApp t' targs

    -- the type of e; the type of constructor; the arguments of constructor; the environment
    addBinding :: LCon -> LCon -> [String] -> TEnv -> Infer TEnv
    addBinding tc ty [] env' | tc == ty = return env'
    addBinding _ _ [] _ = lift $ Left $ LBug "How does this happen? Code 2"
    -- []{x::xs; t1 -> t2} => [x : t1]{xs; t2}
    addBinding tc (LCApp (LCApp LCArr t1) t2) (arg:args') env' = 
      addBinding tc t2 args' env' >>= return . extendEnv arg t1 
    addBinding tc ty args _env = lift $ Left $ LBug $ "How does this happen? Code 3" ++ show (line <> pretty "expression type:" <+> pretty tc <+> pretty "constructor type:" <+> pretty ty <> line) ++ "args: " ++ show args

runInfer :: MTProg -> TEnv -> Univ -> Maybe LErr 
runInfer (Prog stmts) env univ = do
  let errors = Prelude.map (\stmt -> tcStmt stmt env univ) stmts
  case filter (/= Nothing) errors of
    [] -> Nothing
    (err:errs) -> Just $ LMultiErr (Data.List.NonEmpty.map fromJust (err :| errs))

tcStmt :: MTStmt -> TEnv -> Univ -> Maybe LErr
tcStmt (TLExp name ty body) env univ = do 
  case evalStateT (infer body env) 0 of 
    Right (t, cs) -> case unify cs of 
      Just s -> case ty of 
        Just ty' | ty' == applySubstToType s t -> case calcKind ty' univ of 
          Right LKType -> Nothing 
          Right k -> Just $ LExpressionHasWrongKind name ty' k
          Left e -> Just e 
        Just ty' -> Just $ LTErr body ty' (applySubstToType s t)
        Nothing -> Just $ LBug "How does this happen? Code 1"
      Nothing -> Just $ LBug $ "Type inference failed for expression: " ++ name 
    Left err -> Just err
tcStmt (TLEnum _name _targs _variants) _env _univ = Nothing