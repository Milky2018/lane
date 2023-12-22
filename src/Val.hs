{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Val (LVal(..), LBif, VEnv) where

import AST
import Err
import Env
import Data.List (intercalate)
import Pretty (Pretty (pretty))

data LVal 
  = LValInt Int
  | LValString String
  | LValLam String LExpr VEnv
  | LValBif LBif
  | LValStruct String [(String, LVal)]
  | LValEnum String String [LVal]

instance Pretty LVal where
  pretty (LValInt i) = show i
  pretty (LValString s) = s
  pretty (LValLam _ _ _) = "<lambda>"
  pretty (LValBif _) = "<builtin>"
  pretty (LValStruct s fields) = s ++ " {" ++ intercalate "," (map (\(f, v) -> f ++ " = " ++ pretty v) fields) ++ "}"
  pretty (LValEnum enum var []) = enum ++ "." ++ var
  pretty (LValEnum enum var fields) = "(" ++ enum ++ "." ++ var ++ " " ++ unwords (map pretty fields) ++ ")"

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