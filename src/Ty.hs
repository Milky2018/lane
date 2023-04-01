module Ty where

data LType = 
    LTInt 
  | LTString 
  | LTBool 
  | LTUnit 
  | LTLam LType LType
  | LTVar Int
  deriving (Show, Eq) 

pretty :: LType -> String
pretty LTInt = "Int"
pretty LTString = "String"
pretty LTBool = "Bool"
pretty LTUnit = "()"
pretty (LTLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"
pretty (LTVar a) = "TVar" ++ show a

prettyMaybe :: Maybe LType -> String 
prettyMaybe (Just a) = pretty a 
prettyMaybe Nothing = "?"