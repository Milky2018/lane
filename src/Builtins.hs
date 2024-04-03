{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Builtins (NamedBi (..), addBuiltins, addTBuiltins, boolType, unitType, intType, stringType, trueVal, falseVal) where

import Val ( VEnv, LVal(..) )
import AST ()
import Err ( LErr (LBug) )
import Env ( extendEnv )
import Ty ( LType (..), pretty )
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

trueVal :: LVal
trueVal = LValEnum "Bool" "true" []

falseVal :: LVal
falseVal = LValEnum "Bool" "false" []

unitVal :: LVal
unitVal = LValEnum "Unit" "unit" []

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
  , NamedBi "true" boolType trueVal
  , NamedBi "false" boolType falseVal
  , NamedBi "unit" unitType unitVal
  , stringEq
  ]

-- TODO: this should be deleted when Eq a => a -> a -> Bool is supported.
stringEq :: NamedBi
stringEq = NamedBi "eq" t v
  where
    t = LTLam stringType (LTLam stringType boolType)
    v = LValBif $ \(LValString s1) -> return $ LValBif $ \(LValString s2) -> return $ if s1 == s2 then trueVal else falseVal

class CorrespondLValCons a where
  correspond :: (a -> LVal, LType)
  corresback :: LVal -> Maybe a

instance CorrespondLValCons Int where
  correspond = (LValInt, intType)

  corresback (LValInt i) = Just i
  corresback _ = Nothing

instance CorrespondLValCons Bool where
  correspond = (\x -> (if x then trueVal else falseVal), boolType)
  corresback (LValEnum "Bool" "true" []) = Just True
  corresback (LValEnum "Bool" "false" []) = Just False
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
        _ -> Left $ LBug $ "bin op " ++ name ++ " type error." ++ expected ++ actual
          where
            expected = " expected types: " ++ show (pretty (aTy, bTy))
            actual = " actual expressions: " ++ show (pretty (a', b'))
