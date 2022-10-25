module Ty where

data LType = 
    LTypeInt 
  | LTypeString 
  | LTypeBool 
  | LTypeUnit 
  | LTypeLam LType LType
  deriving (Show, Eq)

pretty :: LType -> String
pretty LTypeInt = "Int"
pretty LTypeString = "String"
pretty LTypeBool = "Bool"
pretty LTypeUnit = "()"
pretty (LTypeLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"

prettyMaybe :: Maybe LType -> String 
prettyMaybe (Just a) = pretty a 
prettyMaybe Nothing = "?"