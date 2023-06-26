module Udt (initialTEnv) where

import Ty (UDT, lookupUdt, LTypeVal (..))
import TAST (MTProg, TVEnv, MTStmt)
import Err (LResult, LErr (..))
import AST (TLStmt(..), Prog (..))
import Data.Foldable (foldlM)
import Env (extendEnv)

initialTEnv :: MTProg -> TVEnv -> UDT -> LResult (TVEnv, UDT)
initialTEnv (Prog defs) oldEnv oldUdt = foldlM addDef (oldEnv, oldUdt) defs
  where
    addDef :: (TVEnv, UDT) -> MTStmt -> LResult (TVEnv, UDT)
    addDef (env, udt) (TLExp name (Just ty) _expr) = 
      let tyv = lookupUdt ty udt 
      in return (extendEnv name tyv env, udt)
    addDef _ (TLExp _name Nothing _expr) =
      Left $ LErr "Top level expressions need type annotations"
    -- TODO: add support for recursive types
    addDef (env, udt) (TLStruct struct fields) = do
      fieldTypes <- mapM (\(name, ty) -> case ty of
        Just ty' -> Right (name, lookupUdt ty' udt)
        Nothing -> Left $ LErr "Struct fields need type annotations") fields
      return (env, extendEnv struct (TVStruct struct fieldTypes) udt)

