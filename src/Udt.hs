{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Udt (initialTEnv) where

import Ty (Univ, LCon(..), LKind (..), typeArr)
import TAST (MTProg, TEnv, MTStmt)
import Err (LResult, LErr (..))
import AST (TLStmt(..), Prog (..))
import Data.Foldable (foldlM)
import Env (extendEnv)
import Kind (calcKind)
import Prettyprinter

simpleScan :: Univ -> MTProg -> Univ
simpleScan oldUdt (Prog defs) = foldl addDef oldUdt defs
  where
    addDef :: Univ -> MTStmt -> Univ
    addDef udt (TLEnum name targs _) = extendEnv name (foldr (\_ acc -> LKArr LKType acc) LKType targs) udt
    addDef udt _ = udt

makeForallType :: [String] -> LCon -> LCon
makeForallType [] ty = ty
makeForallType (a:as) ty = LTAll a LKType (makeForallType as ty)

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
    addDef (env, univ) (TLExp name mty _expr) = case mty of 
      Just c -> case calcKind c univ of 
        Right LKType -> return (extendEnv name c env, univ)
        Right k -> Left $ LConstructorIsNotType name k
        Left err -> Left err
      Nothing -> Left $ LTopLevelDefNoAnnotation name 

    -- for "enum E A { var1 (a : A) (a1 : A1), var2 (a2 : A2)}"
    -- add "a1 : <A> A -> A1 -> E" and "a2 : <A> A2 -> E" to the environment
    addDef (env, udt) (TLEnum name targs variants) = do 
      let kind = foldr (\_ acc -> LKArr LKType acc) LKType targs -- kind of E: * -> * 
      let udt' = extendEnv name kind udt 
      let udt'' = foldr (\a acc -> extendEnv a LKType acc) udt' targs -- add A to the environment
      let ty = foldl LTApp (LTId name) (map LTId targs)  -- ty = E[A]
      variants' <- mapM (\(variantName, tys) -> do
        tys' <- mapM (\mty -> case mty of
          Just ty' -> case calcKind ty' (simpleScan udt'' prog) of -- for ty' = A, A1
            Right LKType -> Right ty'
            Right k -> error $ "something wrong. Kind:  " ++ show (pretty k) ++ "; Type: " ++ show (pretty ty')
            -- TODO: support higher kinds
            -- Right k -> Left $ LConstructorIsNotType variantName k
            Left err -> Left err
          Nothing -> Left $ LBug "Enum variants need type annotations") tys
        return (variantName, tys')) variants
      let addVariant env' (varName, tys) = extendEnv varName (makeForallType targs (foldr typeArr ty tys)) env'
      let env' = foldl addVariant env variants'
      return (env', udt')
