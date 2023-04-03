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
  [ bifAdd
  , bifSub
  , bifMul
  , bifDiv
  , bifLt
  , bifGt
  , bifLeq 
  , bifGeq 
  , bifEq
  , bifNeq
  ]

makeBifFromOp :: (LVal -> LVal -> LResult LVal) -> LBif
makeBifFromOp op = Right . LValBif . op

iiiType :: LType
iiiType = LTLam LTInt (LTLam LTInt LTInt)

bifAdd :: NamedBif
bifAdd = NamedBif "+" iiiType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValInt $ a + b
  add _ _ = Left $ LErr "add: expected two integers"

bifSub :: NamedBif
bifSub = NamedBif "-" iiiType $ makeBifFromOp sub where
  sub (LValInt a) (LValInt b) = Right $ LValInt $ a - b
  sub _ _ = Left $ LErr "sub: expected two integers"

bifMul :: NamedBif
bifMul = NamedBif "*" iiiType $ makeBifFromOp mul where
  mul (LValInt a) (LValInt b) = Right $ LValInt $ a * b
  mul _ _ = Left $ LErr "mul: expected two integers"

bifDiv :: NamedBif
bifDiv = NamedBif "/" iiiType $ makeBifFromOp div_ where
  div_ (LValInt a) (LValInt b) = Right $ LValInt $ a `div` b
  div_ _ _ = Left $ LErr "div: expected two integers"

iibType :: LType 
iibType = LTLam LTInt (LTLam LTInt LTBool)

bifLt :: NamedBif
bifLt = NamedBif "<" iibType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValBool $ a < b 
  add _ _ = Left $ LErr "lt: expected two integers"

bifGt :: NamedBif
bifGt = NamedBif ">" iibType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValBool $ a > b 
  add _ _ = Left $ LErr "gt: expected two integers"

bifLeq :: NamedBif
bifLeq = NamedBif "<=" iibType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValBool $ a <= b 
  add _ _ = Left $ LErr "leq: expected two integers"

bifGeq :: NamedBif
bifGeq = NamedBif ">=" iibType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValBool $ a >= b 
  add _ _ = Left $ LErr "geq: expected two integers"

bifEq :: NamedBif
bifEq = NamedBif "==" iibType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValBool $ a == b 
  add _ _ = Left $ LErr "eq: expected two integers"

bifNeq :: NamedBif
bifNeq = NamedBif "!=" iibType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValBool $ a /= b 
  add _ _ = Left $ LErr "neq: expected two integers"