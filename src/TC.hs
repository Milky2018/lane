{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Eta reduce" #-}
module TC (typeCheck, elimTypeProg) where
import AST ( Expr(..), LProg, Prog (..), TLStmt (..), transProg )
import Builtins ( addTBuiltins )
import Env ( emptyEnv, lookupEnv, extendEnv )
import Err ( LResult, LErr(..) )
import TAST ( MTProg, TVEnv, TVProg, TVStmt, TVExpr, lookallup )
import Ty ( LTypeVal (..), LType, UDT )
import Data.Maybe (fromMaybe, fromJust)
import Builtintypes (addBuiltinTypes)
import Udt (initialTEnv)
import Control.Monad (foldM, foldM_)
import Pretty (pretty)

elimType :: Maybe LType -> ()
elimType _ = ()

elimTypeProg :: MTProg -> LProg
elimTypeProg = transProg elimType

-- Typecheck a program, and maybe returns an error if the program does not 
-- typecheck or has some other errors. 
typeCheck :: MTProg -> Maybe LErr
typeCheck prog =
  case initialTEnv prog (addTBuiltins emptyEnv) (addBuiltinTypes emptyEnv) of
    Right (tenv, udt) -> tcProg (lookallup udt prog) tenv udt
    Left err -> Just err

tcProg :: TVProg -> TVEnv -> UDT -> Maybe LErr
tcProg (Prog stmts) env udt = tcStmts stmts env udt

tcStmts :: [TVStmt] -> TVEnv -> UDT -> Maybe LErr
tcStmts ss env udt = let errors = map (\stmt -> tcStmt stmt env udt) ss in
  case filter (/= Nothing) errors of
    [] -> Nothing
    errs -> Just $ head $ map fromJust errs

tcStmt :: TVStmt -> TVEnv -> UDT -> Maybe LErr
tcStmt (TLExp _name ty body) env udt =
  case tc body ty env udt of
    Right _ -> Nothing
    Left err -> Just err
tcStmt (TLStruct _struct _fields) _env _udt = Nothing
tcStmt (TLEnum _name _variants) _env _udt = Nothing

-- This type checker uses a simple bidirectional algorithm
-- tc :: expr -> expected type -> type env -> udt -> result 
tc :: TVExpr -> Maybe LTypeVal -> TVEnv -> UDT -> LResult LTypeVal
tc (EInt _) (Just TVInt) _ _ = return TVInt
tc (EInt n) (Just t) _ _ = Left $ LTErr (EInt n) t TVInt
tc (EInt _) Nothing _ _ = return TVInt

tc (EString _) (Just TVString) _ _ = return TVString
tc (EString s) (Just t) _ _ = Left $ LTErr (EString s) t TVString
tc (EString _) Nothing _ _ = return TVString

tc (EId x) t env _udt = case lookupEnv x env of
  Just t' -> case t of
    Just t'' | t'' == t' -> return t'
    Just t'' -> Left $ LTErr (EId x) t'' t'
    Nothing -> return t'
  Nothing -> Left $ LErr ("Unbound variable: " ++ x)

tc (EApp e1 e2) t env udt = do
  t2 <- tc e2 Nothing env udt
  case t of
    Just t' -> do
      t1 <- tc e1 (Just (TVLam t2 t')) env udt
      if t1 == TVLam t2 t' then return t' else Left $ LBug "This should never happend"
    Nothing -> do
      t1 <- tc e1 Nothing env udt
      case t1 of
        TVLam t1' t1'' | t1' == t2 -> return t1''
        TVLam t1' _t1'' -> Left $ LTErr e2 t1' t2
        t'' -> Left $ LTErr (EApp e1 e2) t2 t''

tc (ELam x mt1 e mt2) Nothing env udt = do
  t1 <- case mt1 of
    Just t  -> return t
    Nothing -> Left $ LErr ("Missing type annotation for argument: " ++ x)
  let env' = extendEnv x t1 env
  t2 <- tc e mt2 env' udt
  return (TVLam t1 t2)
tc (ELam x mt1 e mt2) (Just (TVLam t1 t2)) env udt = do
  let eShouldType = tc e (Just t2) (extendEnv x t1 env) udt
  _ <- case (mt1, mt2) of
    (Just t1', Just t2') | t1 == t1' && t2 == t2' -> eShouldType
    (Just t1', Nothing) | t1 == t1' -> eShouldType
    (Nothing, Just t2') | t2 == t2' -> eShouldType
    (Nothing, Nothing) -> eShouldType
    _ -> Left $ LTErr (ELam x mt1 e mt2) (TVLam t1 t2) (TVLam (fromMaybe t1 mt1) (fromMaybe t2 mt2))
  return (TVLam t1 t2)
tc (ELam _x _mt1 _e _mt2) (Just t) _ _ = Left $
  LErr $ "Expect a function type, but got" ++ show t

-- tc fix@(EFix f mt1 e mt2) Nothing env = 
--   case (mt1, mt2) of 
--     (Just t1, Just t2) | t1 == t2 -> return t1
--     (Just t1, Just t2) -> Left $ LTErr (EFix f mt1 e mt2) t1 t2
--     (Just t1, Nothing) -> do 
--       t2 <- tc e (Just t1) (extendEnv f t1 env)
--       if t1 == t2 then return t1 else Left $ LTErr (EFix f mt1 e mt2) t1 t2
--     (Nothing, Just t2) -> return t2
--     (Nothing, Nothing) -> Left $ LErr $ "Missing type annotation for argument: " ++ f ++ " in " ++ pretty fix
-- tc (EFix f mt1 e mt2) (Just t) env =
--   case (mt1, mt2) of 
--     (Just t1, Just t2) | t1 == t2 && t == t1 -> return t1
--     (Just t1, Just t2) | t1 == t2 -> Left $ LTErr (EFix f mt1 e mt2) t t1
--     (Just t1, Just t2) -> Left $ LTErr (EFix f mt1 e mt2) t1 t2
--     (Just t1, Nothing) | t == t1 -> do 
--       t2 <- tc e (Just t1) (extendEnv f t1 env)
--       if t1 == t2 then return t1 else Left $ LTErr (EFix f mt1 e mt2) t1 t2
--     (Just t1, Nothing) -> Left $ LTErr (EFix f mt1 e mt2) t t1
--     (Nothing, Just t2) | t == t2 -> return t2
--     (Nothing, Just t2) -> Left $ LTErr (EFix f mt1 e mt2) t t2
--     (Nothing, Nothing) -> do 
--       t2 <- tc e (Just t) (extendEnv f t env)
--       if t == t2 then return t else Left $ LTErr (EFix f mt1 e mt2) t t2

tc (ELetrec bindings e) should env udt = do
  newEnv <- foldM
      (\env' (name, mt, _expr) -> case mt of
        Just t -> return $ extendEnv name t env'
        Nothing -> Left $ LErr $ "Missing type annotation for argument: " ++ name ++ " in " ++ pretty (ELetrec bindings e))
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
  _t1 <- tc e1 (Just TVBool) env udt
  t2 <- tc e2 t env udt
  t3 <- tc e3 t env udt
  if t2 == t3 then return t2 else Left $ LTErr (EIf e1 e2 e3) t2 t3

tc (EStruct n fields) Nothing env udt = do
  fieldTypes <- mapM fieldType fields
  definedN <- case lookupEnv n udt of
    Just (TVStruct name fields') -> return (TVStruct name fields')
    Just tv -> Left $ LTErr (EStruct n fields) (TVStruct n fieldTypes) tv
    Nothing -> Left $ LErr $ "Struct " ++ n ++ " not found"
  if definedN == TVStruct n fieldTypes
  then return definedN
  else Left $ LTErr (EStruct n fields) (TVStruct n fieldTypes) definedN
  where
    fieldType (field, expr) = do
      t <- tc expr Nothing env udt
      return (field, t)

-- TODO: in fact, it is enough to check only the `should` record name with `n`
tc (EStruct n fields) (Just should) env udt =
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
              t <- tc expr (Just expect) env udt
              return (field, t)
            Nothing -> Left $ LTFiledNotFound field
    _ -> Left $ LTErr (EStruct n fields) should (TVStruct n [])

tc (EEnum enumName varName fields) Nothing env udt = do
  variants <- case lookupEnv enumName udt of
    Just (TVEnum _name variants) -> return variants
    Just tv -> Left $ LTErr (EEnum enumName varName fields) (TVEnum enumName []) tv
    Nothing -> Left $ LErr $ "Enum " ++ enumName ++ " not found"
  case lookup varName variants of
    Just argTypes -> do
      let argMaybeTypes = Just <$> argTypes
      if length fields /= length argTypes
      then Left $ LTErr (EEnum enumName varName fields) (TVEnum enumName variants) (TVEnum enumName [])
      else do
        argTypes' <- mapM fieldType (zip fields argMaybeTypes)
        if argTypes == argTypes'
        then return (TVEnum enumName variants)
        else Left $ LTErr (EEnum enumName varName fields) (TVEnum enumName variants) (TVEnum enumName [(varName, argTypes')])
      where
        fieldType (expr, should) = do
          tc expr should env udt
    Nothing -> Left $ LTErr (EEnum enumName varName fields) (TVEnum enumName []) (TVEnum enumName [])

tc (EEnum enumName varName fields) (Just should) _env _udt = do 
  case should of 
    TVEnum name _varaints | name == enumName -> return should 
    _ -> Left $ LTErr (EEnum enumName varName fields) should (TVEnum enumName [])

tc (EAccess expr field) should env udt = do
  t <- tc expr Nothing env udt
  case t of
    TVStruct _name fields -> case lookup field fields of
      Just t' -> case should of
        Just t'' | t'' == t' -> return t'
        Just t'' -> Left $ LTErr (EAccess expr field) t'' t'
        Nothing -> return t'
      Nothing -> Left $ LTFiledNotFound field
    _ -> Left $ LTErr (EAccess expr field) (TVStruct "<struct>" []) t