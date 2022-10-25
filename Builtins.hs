module Builtins where

import Val
import AST
import Err
import Env
import Ty
import TAST

data NamedBif = NamedBif {
    name :: String
  , ty :: LType
  , bif :: LBif
}

addBuiltin :: NamedBif -> VEnv -> VEnv
addBuiltin (NamedBif name _ bif) = extendEnv name (LValBif bif)

addBuiltins :: VEnv -> VEnv
addBuiltins env = foldr addBuiltin env builtins

addTBuiltin :: NamedBif -> TEnv -> TEnv
addTBuiltin (NamedBif name ty _) = extendEnv name ty 

addTBuiltins :: TEnv -> TEnv
addTBuiltins env = foldr addTBuiltin env builtins

builtins = [bifAdd, bifSub, bifMul, bifDiv]

makeBifFromOp :: (LVal -> LVal -> LResult LVal) -> LBif
makeBifFromOp op = Right . LValBif . op

iiiType = LTypeLam LTypeInt (LTypeLam LTypeInt LTypeInt)

bifAdd = NamedBif "+" iiiType $ makeBifFromOp add where
  add (LValInt a) (LValInt b) = Right $ LValInt $ a + b
  add _ _ = Left $ LErr "add: expected two integers"

bifSub = NamedBif "-" iiiType $ makeBifFromOp sub where
  sub (LValInt a) (LValInt b) = Right $ LValInt $ a - b
  sub _ _ = Left $ LErr "sub: expected two integers"

bifMul = NamedBif "*" iiiType $ makeBifFromOp mul where
  mul (LValInt a) (LValInt b) = Right $ LValInt $ a * b
  mul _ _ = Left $ LErr "mul: expected two integers"

bifDiv = NamedBif "/" iiiType $ makeBifFromOp div_ where
  div_ (LValInt a) (LValInt b) = Right $ LValInt $ a `div` b
  div_ _ _ = Left $ LErr "div: expected two integers"


