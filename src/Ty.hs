{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Ty (LType (..), pretty, UDT) where
import Pretty (Pretty (pretty))

data LType = 
    LTLam LType LType
  | LTId String 
  deriving (Show, Eq) 

type UDT = [String]

instance Pretty LType where 
  pretty (LTLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"
  pretty (LTId name) = name
