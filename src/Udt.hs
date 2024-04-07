{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Udt (initialTEnv) where

import Ty (UDT, LType(..))
import TAST (MTProg, TEnv, MTStmt)
import Err (LResult, LErr (..))
import AST (TLStmt(..), Prog (..))
import Data.Foldable (foldlM)
import Env (extendEnv)

simpleScan :: UDT -> MTProg -> UDT
simpleScan oldUdt (Prog defs) = foldl addDef oldUdt defs
  where
    addDef :: UDT -> MTStmt -> UDT
    addDef udt (TLEnum name _) = name : udt
    addDef udt _ = udt

-- Add top level definitions to the type environment. Now, the top level 
-- definitions will be added one by one, which does not support mutual 
-- recursion. 
-- Usage: initialTEnv [exp1, func1, struct1] [+', -', *'] [Int', String', Bool'] =>
--        ( [+', -', *', exp1', func1']
--          [Int', String', Bool', struct1'] )
initialTEnv :: MTProg -> TEnv -> UDT -> LResult (TEnv, UDT)
initialTEnv prog@(Prog defs) oldEnv oldUdt = foldlM addDef (oldEnv, oldUdt) defs
  where
    addDef :: (TEnv, UDT) -> MTStmt -> LResult (TEnv, UDT)
    addDef (env, udt) (TLExp name mty _expr) = case mty of 
      Just ty -> return (extendEnv name ty env, udt)
      Nothing -> Left $ LTopLevelDefNoAnnotation name 

    addDef (env, udt) (TLEnum name variants) = do 
      let udt' = name : udt 
      let ty = LTId name 
      variants' <- mapM (\(variantName, tys) -> do
        tys' <- mapM (\mty -> case mty of
          Just ty' | ty' `typeIn` simpleScan oldUdt prog -> Right ty'
          Just ty' -> Left $ LTypeNotInEnv ty'
          Nothing -> Left $ LBug "Enum variants need type annotations") tys
        return (variantName, tys')) variants
      let addVariant env' (varName, tys) = extendEnv varName (foldr LTLam ty tys) env'
      let env' = foldl addVariant env variants'
      return (env', udt')
      
typeIn :: LType -> UDT -> Bool 
typeIn (LTId name) udt = name `elem` udt
typeIn (LTLam t1 t2) udt = typeIn t1 udt && typeIn t2 udt