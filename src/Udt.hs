{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Udt (initialTEnv) where

import Ty (Univ, LCon(..), calcKind, LKind (..), typeArr)
import TAST (MTProg, TEnv, MTStmt)
import Err (LResult, LErr (..))
import AST (TLStmt(..), Prog (..))
import Data.Foldable (foldlM)
import Env (extendEnv)

simpleScan :: Univ -> MTProg -> Univ
simpleScan oldUdt (Prog defs) = foldl addDef oldUdt defs
  where
    addDef :: Univ -> MTStmt -> Univ
    addDef udt (TLEnum name _) = extendEnv name LKType udt
    addDef udt _ = udt

-- Add top level definitions to the type environment. Now, the top level 
-- definitions will be added one by one, which does not support mutual 
-- recursion. 
-- Usage: initialTEnv [exp1, func1, struct1] [+', -', *'] [Int', String', Bool'] =>
--        ( [+', -', *', exp1', func1']
--          [Int', String', Bool', struct1'] )
initialTEnv :: MTProg -> TEnv -> Univ -> LResult (TEnv, Univ)
initialTEnv prog@(Prog defs) oldEnv oldUdt = foldlM addDef (oldEnv, oldUdt) defs
  where
    addDef :: (TEnv, Univ) -> MTStmt -> LResult (TEnv, Univ)
    addDef (env, udt) (TLExp name mty _expr) = case mty of 
      Just ty -> return (extendEnv name ty env, udt)
      Nothing -> Left $ LTopLevelDefNoAnnotation name 

    addDef (env, udt) (TLEnum name variants) = do 
      let udt' = extendEnv name LKType udt
      let ty = LTId name 
      variants' <- mapM (\(variantName, tys) -> do
        tys' <- mapM (\mty -> case mty of
          Just ty' | ty' `isType` simpleScan oldUdt prog -> Right ty'
          Just ty' -> Left $ LTypeNotInEnv ty'
          Nothing -> Left $ LBug "Enum variants need type annotations") tys
        return (variantName, tys')) variants
      let addVariant env' (varName, tys) = extendEnv varName (foldr typeArr ty tys) env'
      let env' = foldl addVariant env variants'
      return (env', udt')

isType :: LCon -> Univ -> Bool 
isType c univ = calcKind c univ == LKType