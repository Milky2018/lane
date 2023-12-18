{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Udt (initialTEnv) where

import Ty (UDT, lookupUdt, LTypeVal (..))
import TAST (MTProg, TVEnv, MTStmt)
import Err (LResult, LErr (..))
import AST (TLStmt(..), Prog (..))
import Data.Foldable (foldlM)
import Env (extendEnv)

-- Add top level definitions to the type environment. Now, the top level 
-- definitions will be added one by one, which does not support mutual 
-- recursion. 
-- Usage: initialTEnv [exp1, func1, struct1] [+', -', *'] [Int', String', Bool'] =>
--        ( [+', -', *', exp1', func1']
--          [Int', String', Bool', struct1'] )
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
      -- return (extendEnv struct (TVStruct struct fieldTypes) env, extendEnv struct (TVStruct struct fieldTypes) udt)
      return (env, extendEnv struct (TVStruct struct fieldTypes) udt)
    -- addDef (env, udt) (TLEnum name variants) = do 
    --   variantTypes <- mapM (\(variantName, tys) -> do
    --     tys' <- mapM (\ty -> case ty of
    --       Just ty' -> Right (lookupUdt ty' udt)
    --       Nothing -> Left $ LErr "Enum variants need type annotations") tys
    --     return (variantName, tys')) variants
    --   return (env, extendEnv name (TVEnum name variantTypes) udt)
    addDef (env, udt) (TLEnum name variants) = do 
      variantTypes <- mapM (\(variantName, tys) -> do
        tys' <- mapM (\ty -> case ty of
          Just ty' -> Right (lookupUdt ty' udt)
          Nothing -> Left $ LErr "Enum variants need type annotations") tys
        return (variantName, tys')) variants
      return (env, extendEnv name (TVEnum name variantTypes) udt)