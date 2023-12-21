module Builtintypes (addBuiltinTypes) where
import Ty (UDT)

builtinTypes :: UDT 
builtinTypes = ["Bool", "Int", "String", "Unit"]

-- Add builtin types to the type environment. 
-- Usage: addBuiltinTypes [] => 
--  [Bool |-> TVBool, 
--   Int  |-> TVInt, ... ]
addBuiltinTypes :: UDT -> UDT
addBuiltinTypes udt = udt ++ builtinTypes
