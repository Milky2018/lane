module Builtintypes (addBuiltinTypes) where
import Ty (UDT, LTypeVal (..))
import Env (extendEnv)

builtinTypes :: [(String, LTypeVal)]
builtinTypes = [
    ("Bool", TVBool)
  , ("Int", TVInt)
  , ("String", TVString)
  , ("Unit", TVUnit)
  ]

addBuiltinTypes :: UDT -> UDT
addBuiltinTypes udts = foldr (uncurry extendEnv) udts builtinTypes
