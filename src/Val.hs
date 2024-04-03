{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Val (LVal(..), LBif, VEnv) where

import AST
import Err
import Env
import Prettyprinter

data LVal 
  = LValInt Int
  | LValString String
  | LValLam String LExpr VEnv
  | LValBif LBif
  | LValStruct String [(String, LVal)]
  | LValEnum String String [LVal]

instance Pretty LVal where
  pretty (LValInt i) = pretty i
  pretty (LValString s) = pretty s
  pretty (LValLam _ _ _) = pretty "<lambda>"
  pretty (LValBif _) = pretty "<builtin>"
  pretty (LValStruct s fields) = hsep [pretty s, pretty "{", hsep $ punctuate comma (map (\(f, v) -> pretty f <+> pretty "=" <+> pretty v) fields), pretty "}"]
  pretty (LValEnum enum var []) = hcat [pretty enum, pretty ".", pretty var]
  pretty (LValEnum enum var fields) = parens $ hcat [pretty enum, pretty ".", pretty var] <+> hsep (map pretty fields)

type VEnv = Env String LVal

type LBif = LVal -> LResult LVal

instance Eq LVal where
  (LValInt i1) == (LValInt i2) = i1 == i2
  (LValString s1) == (LValString s2) = s1 == s2
  (LValLam _ _ _) == (LValLam _ _ _) = False
  (LValBif _) == (LValBif _) = False
  (LValEnum e1 v1 fs1) == (LValEnum e2 v2 fs2) = e1 == e2 && v1 == v2 && fs1 == fs2
  (LValStruct s1 fs1) == (LValStruct s2 fs2) = s1 == s2 && fs1 == fs2
  _ == _ = False