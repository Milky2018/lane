{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Ty (LType (..), pretty, UDT) where

import Prettyprinter

data LType = 
    LTLam LType LType
  | LTId String 
  deriving (Eq) 

type UDT = [String]

instance Pretty LType where 
  pretty (LTLam t1 t2) = parens $ pretty t1 <+> pretty "->" <+> pretty t2
  pretty (LTId name) = pretty name

