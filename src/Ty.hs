module Ty (LType (..), pretty) where
import Pretty (Pretty (pretty))

data LType = 
    LTInt 
  | LTString 
  | LTBool 
  | LTUnit 
  | LTLam LType LType
  | LTVar Int
  deriving (Show, Eq) 

instance Pretty LType where 
  pretty LTInt = "Int"
  pretty LTString = "String"
  pretty LTBool = "Bool"
  pretty LTUnit = "()"
  pretty (LTLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"
  pretty (LTVar a) = "TVar" ++ show a
