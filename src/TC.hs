{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module TC (typeCheck, elimType) where
import AST ( LExpr, Expr(..), LProg, Prog (..), TLStmt (..), LTLStmt )
import Builtins ( addTBuiltins )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Err ( LResult, LErr(..) )
import TAST ( TEnv, MTProg, MTExpr, MTStmt )
import Ty ( LType(..), UDT )
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Bifunctor
import Data.Foldable (foldlM)
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
elimTypeExpr (EIf e1 e2 e3) = EIf (elimTypeExpr e1) (elimTypeExpr e2) (elimTypeExpr e3)
elimTypeExpr (EAccess e field) = EAccess (elimTypeExpr e) field
elimTypeExpr (EStruct name fields) = EStruct name (map (Data.Bifunctor.second elimTypeExpr) fields)

initialTEnv :: MTProg -> TEnv -> UDT -> LResult (TEnv, UDT)
initialTEnv (Prog defs) oldEnv oldUdt = foldlM addDef (oldEnv, oldUdt) defs
  where
    addDef :: (TEnv, UDT) -> MTStmt -> LResult (TEnv, UDT)
    addDef (env, udt) (TLExp name (Just ty) _expr) = Right (extendEnv name ty env, udt)
    addDef _ (TLExp name Nothing _expr) = Left $ LErr $ "Top level definition " ++ name ++ " needs type annotaions"
    addDef (env, udt) (TLStruct struct fields) = do 
      fieldTypes <- mapM (\(name, ty) -> case ty of
        Just ty' -> Right (name, ty')
        Nothing -> Left $ LErr "Struct fields need type annotations") fields
      return (env, extendEnv struct (LTStruct struct fieldTypes) udt)

typeCheck :: MTProg -> Maybe LErr
typeCheck prog@(Prog stmts) = 
  case initialTEnv prog (addTBuiltins emptyEnv) emptyEnv of 
    Right (tenv, udt) -> tcStmts stmts tenv udt
    Left err -> Just err

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
tcStmt (TLStruct _struct _fields) _env _udt = Nothing

-- This type checker uses a simple bidirectional algorithm
-- tc :: expr -> expected type -> type env -> udt -> result 
tc :: MTExpr -> Maybe LType -> TEnv -> UDT -> LResult LType
tc (EInt _) (Just LTInt) _ _ = return LTInt
tc (EInt n) (Just t) _ _ = Left $ LTErr (EInt n) t LTInt
tc (EInt _) Nothing _ _ = return LTInt

tc (EString _) (Just LTString) _ _ = return LTString
tc (EString s) (Just t) _ _ = Left $ LTErr (EString s) t LTString
tc (EString _) Nothing _ _ = return LTString

tc (EId x) t env _ = case lookupEnv x env of
  Just t' -> case t of
    Just t'' | t'' == t' -> return t'
    Just t'' -> Left $ LTErr (EId x) t'' t'
    Nothing -> return t'
  Nothing -> Left $ LErr ("Unbound variable: " ++ x)

tc (EApp e1 e2) t env udt = do
  t2 <- tc e2 Nothing env udt
  case t of
    Just t' -> do
      t1 <- tc e1 (Just (LTLam t2 t')) env udt
      if t1 == LTLam t2 t' then return t' else Left $ LBug "This should never happend"
    Nothing -> do
      t1 <- tc e1 Nothing env udt
      case t1 of
        LTLam t1' t1'' | t1' == t2 -> return t1''
        LTLam t1' _t1'' -> Left $ LTErr e2 t1' t2
        t'' -> Left $ LTErr (EApp e1 e2) t2 t'' 

tc (ELam x mt1 e mt2) Nothing env udt = do
  t1 <- case mt1 of
    Just t  -> return t
    Nothing -> Left $ LErr ("Missing type annotation for argument: " ++ x)
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
tc (ELam x mt1 e mt2) (Just t) _ _ = Left $ 
  LTErr (ELam x mt1 e mt2) 
        t 
        (LTLam (fromMaybe (LTId "<unknown>") mt1) (fromMaybe (LTId "<unknown>") mt2))

tc (EIf e1 e2 e3) t env udt = do
  _t1 <- tc e1 (Just LTBool) env udt
  t2 <- tc e2 t env udt
  t3 <- tc e3 t env udt
  if t2 == t3 then return t2 else Left $ LTErr (EIf e1 e2 e3) t2 t3

tc (EStruct n fields) Nothing env udt =
  case lookupEnv n udt of 
    Nothing -> Left $ LErr $ "Unbound struct: " ++ n
    Just (LTStruct _name userDefStruct) -> 
      if length userDefStruct /= length fields then Left $ LErr $ "Struct " ++ n ++ " has " ++ show (length userDefStruct) ++ " fields, but " ++ show (length fields) ++ " were given"
      else do 
        _fieldTypes <- mapM checkField userDefStruct
        return (LTId n)
        where checkField (field, t) = case lookup field fields of 
                Nothing -> Left $ LErr $ "Field " ++ field ++ " not found in struct " ++ n
                Just expr -> do 
                  t' <- tc expr (Just t) env udt
                  if t == t' then return (field, t) else Left $ LTErr expr t t'
    Just t -> Left $ LTErr (EStruct n fields) (LTStruct n []) t
tc (EStruct n fields) (Just should) env udt = 
  if should == LTId n 
  then tc (EStruct n fields) Nothing env udt 
  else Left $ LTErr (EStruct n fields) should (LTId n)

tc (EAccess expr field) should env udt = do
  t <- tc expr Nothing env udt
  case t of
    LTId i -> case lookupEnv i udt of
      Just (LTStruct _name fields) -> case lookup field fields of
        Just t' -> case should of
          Just t'' | t'' == t' -> return t'
          Just t'' -> Left $ LTErr (EAccess expr field) t'' t'
          Nothing -> return t'
        Nothing -> Left $ LErr ("Field " ++ field ++ " not found in record")
      Just t' -> Left $ LTErr (EAccess expr field) (LTStruct "<struct>" []) t'
      Nothing -> Left $ LErr ("Unbound type: " ++ i)
    _ -> Left $ LTErr (EAccess expr field) (LTId "<struct>") t