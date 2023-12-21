module Builtins (NamedBi (..), addBuiltins, addTBuiltins, boolType, unitType, intType, stringType) where

import Val ( VEnv, LVal(..) )
import AST ()
import Err ( LErr(LErr) )
import Env ( extendEnv )
import Ty ( LType (..) )
import Pretty (pretty)
import qualified TAST

data NamedBi = NamedBi String LType LVal 

-- Add a builtin function to a value environment. For example, function "+" in 
-- Lane is added to the value environment as a function in the interpreter 
-- that just does the addition.
--
-- Usage: addBuiltin - [+'] => [-', +']. The -' and +' means a function equipped  
-- with its interpreted value. 
addBuiltin :: NamedBi -> VEnv -> VEnv
addBuiltin (NamedBi name _ bi) = extendEnv name bi

addBuiltins :: VEnv -> VEnv
addBuiltins env = foldr addBuiltin env builtins

-- Add a builtin function to a type environment. For example, function "+" in 
-- Lane is added to the type environment with type Int -> Int -> Int. 
addTBuiltin :: NamedBi -> TAST.TEnv -> TAST.TEnv
addTBuiltin (NamedBi name ty _) = extendEnv name ty

addTBuiltins :: TAST.TEnv -> TAST.TEnv
addTBuiltins env = foldr addTBuiltin env builtins

boolType :: LType
boolType = LTId "Bool"

unitType :: LType
unitType = LTId "Unit"

intType :: LType
intType = LTId "Int"

stringType :: LType 
stringType = LTId "String"

builtins :: [NamedBi]
builtins =
  [ makeBifFromBinOp "+" ((+) :: Int -> Int -> Int)
  , makeBifFromBinOp "-" ((-) :: Int -> Int -> Int)
  , makeBifFromBinOp "*" ((*) :: Int -> Int -> Int)
  , makeBifFromBinOp "/" (div :: Int -> Int -> Int)
  , makeBifFromBinOp "<" ((<) :: Int -> Int -> Bool)
  , makeBifFromBinOp ">" ((>) :: Int -> Int -> Bool)
  , makeBifFromBinOp "<=" ((<=) :: Int -> Int -> Bool)
  , makeBifFromBinOp ">=" ((>=) :: Int -> Int -> Bool)
  -- TODO: support Eq a => a -> a -> Bool 
  , makeBifFromBinOp "==" ((==) :: Int -> Int -> Bool)
  , makeBifFromBinOp "!=" ((/=) :: Int -> Int -> Bool)
  , NamedBi "true" boolType (LValBool True)
  , NamedBi "false" boolType (LValBool False)
  , NamedBi "unit" unitType LValUnit
  ]

class CorrespondLValCons a where
  correspond :: (a -> LVal, LType)
  corresback :: LVal -> Maybe a

instance CorrespondLValCons Int where
  correspond = (LValInt, intType)

  corresback (LValInt i) = Just i
  corresback _ = Nothing

instance CorrespondLValCons Bool where
  correspond = (LValBool, boolType)

  corresback (LValBool b) = Just b
  corresback _ = Nothing

makeBifFromBinOp :: forall a b c. 
                    (CorrespondLValCons a, CorrespondLValCons b, CorrespondLValCons c) => 
                    String -> (a -> b -> c) -> NamedBi
makeBifFromBinOp name op = 
  let (_aV, aTy) = correspond @a
      (_bV, bTy) = correspond @b 
      (cV, cTy) = correspond
  in NamedBi name (LTLam aTy (LTLam bTy cTy)) $
    LValBif $ (\p -> Right . LValBif . p) $
      \a' b' -> case (corresback a', corresback b') of
        (Just a'', Just b'') -> Right $ cV $ op a'' b''
        _ -> Left $ LErr $ "bin op " ++ name ++ " type error." ++ expected ++ actual
          where
            expected = " expected types: " ++ pretty (aTy, bTy)
            actual = " actual expressions: " ++ pretty (a', b')
