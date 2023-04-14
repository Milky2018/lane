module Builtins (NamedBif (..), addBuiltins, addTBuiltins) where

import Val ( LBif, VEnv, LVal(LValInt, LValBif, LValBool) )
import AST ()
import Err ( LResult, LErr(LErr) )
import Env ( extendEnv )
import Ty ( LType(..) )
import TAST ( TEnv )

data NamedBif = NamedBif {
    getName :: String
  , getTy :: LType
  , getBif :: LBif
}

addBuiltin :: NamedBif -> VEnv -> VEnv
addBuiltin (NamedBif name _ bif) = extendEnv name (LValBif bif)

addBuiltins :: VEnv -> VEnv
addBuiltins env = foldr addBuiltin env builtins

addTBuiltin :: NamedBif -> TAST.TEnv -> TAST.TEnv
addTBuiltin (NamedBif name ty _) = extendEnv name ty

addTBuiltins :: TAST.TEnv -> TAST.TEnv
addTBuiltins env = foldr addTBuiltin env builtins

builtins :: [NamedBif]
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

makeBifFromBinOp :: forall a b c. (CorrespondLValCons a, CorrespondLValCons b, CorrespondLValCons c) => String -> (a -> b -> c) -> NamedBif
makeBifFromBinOp name op = 
  let (_aV, aTy) = correspond @a
      (_bV, bTy) = correspond @b 
      (cV, cTy) = correspond
  in NamedBif name (LTLam aTy (LTLam bTy cTy)) $
    (\p -> Right . LValBif . p) $
      \a' b' -> case (corresback a', corresback b') of
        (Just a'', Just b'') -> Right $ cV $ op a'' b''
        _ -> Left $ LErr $ name ++ " type error." ++ expected ++ actual
          where
            expected = " expected: " ++ show (aTy, bTy)
            actual = " actual: " ++ show (a', b')
