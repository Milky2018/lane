module Builtintypes (addBuiltinTypes) where
import Ty (UDT, LType (..))
import Env (extendEnv)

builtinTypes :: [(String, LType)]
builtinTypes = [
    ("Bool", LTBool)
  , ("Int", LTInt)
  , ("String", LTString)
  , ("Unit", LTUnit)
  ]

addBuiltinTypes :: UDT -> UDT
addBuiltinTypes udts = foldr (uncurry extendEnv) udts builtinTypes
