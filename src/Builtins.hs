module Builtins (NamedBi (..), addBuiltins, addTBuiltins) where

import Val ( VEnv, LVal(..) )
import AST ()
import Err ( LErr(LErr) )
import Env ( extendEnv )
import Ty ( LType(..) )
import TAST ( TEnv )

data NamedBi = NamedBi String LType LVal 

addBuiltin :: NamedBi -> VEnv -> VEnv
addBuiltin (NamedBi name _ bi) = extendEnv name bi

addBuiltins :: VEnv -> VEnv
addBuiltins env = foldr addBuiltin env builtins

addTBuiltin :: NamedBi -> TAST.TEnv -> TAST.TEnv
addTBuiltin (NamedBi name ty _) = extendEnv name ty

addTBuiltins :: TAST.TEnv -> TAST.TEnv
addTBuiltins env = foldr addTBuiltin env builtins

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
  , NamedBi "true" LTBool (LValBool True)
  , NamedBi "false" LTBool (LValBool False)
  , NamedBi "unit" LTUnit LValUnit
  ]

class CorrespondLValCons a where
  correspond :: (a -> LVal, LType)
  corresback :: LVal -> Maybe a

instance CorrespondLValCons Int where
  correspond = (LValInt, LTInt)

  corresback (LValInt i) = Just i
  corresback _ = Nothing

instance CorrespondLValCons Bool where
  correspond = (LValBool, LTBool)

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
        _ -> Left $ LErr $ name ++ " type error." ++ expected ++ actual
          where
            expected = " expected: " ++ show (aTy, bTy)
            actual = " actual: " ++ show (a', b')
