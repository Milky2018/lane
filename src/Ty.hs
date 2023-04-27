module Ty (LType (..), pretty, UDT) where
import Pretty (Pretty (pretty))
import Env

data LType = 
    LTInt 
  | LTString 
  | LTBool 
  | LTUnit 
  | LTStruct String [(String, LType)]
  | LTLam LType LType
  | LTId String 
  deriving (Show, Eq) 

type UDT = Env String LType 

data LTypeVal = 
    TVInt 
  | TVString
  | TVBool
  | TVUnit
  | TVStruct String [(String, LTypeVal)]
  | TVLam LTypeVal LTypeVal
  deriving (Show, Eq)

instance Pretty LType where 
  pretty LTInt = "Int"
  pretty LTString = "String"
  pretty LTBool = "Bool"
  pretty LTUnit = "()"
  pretty (LTLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"
  pretty (LTId name) = name
  pretty (LTStruct name fields) = "struct " ++ name ++ " {" ++ unwords (map (\(f, t) -> f ++ " : " ++ pretty t) fields) ++ "}"