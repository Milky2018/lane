module Builtintypes (addBuiltinTypes) where
import Ty (Univ, LKind (..))
import Env (extendEnv)

builtinTypes :: [(String, LKind)] 
builtinTypes = 
  [ ("Bool", LKType)
  , ("Int", LKType)
  , ("String", LKType)
  , ("Unit", LKType)
  ]

addBuiltinTypes :: Univ -> Univ
addBuiltinTypes udt = foldr addType udt builtinTypes
  where
    addType :: (String, LKind) -> Univ -> Univ
    addType (name, kind) = extendEnv name kind
