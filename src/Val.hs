{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Val (LVal(..), LBif, VEnv) where 

import AST 
import Err 
import Env

data LVal = 
    LValBool Bool 
  | LValInt Int 
  | LValUnit
  | LValString String
  | LValLam String LExpr VEnv 
  | LValBif LBif 
  | LValStruct String [(String, LVal)]

instance Show LVal where 
  show (LValBool b) = show b 
  show (LValInt i) = show i 
  show LValUnit = "()" 
  show (LValString s) = show s 
  show (LValLam _ _ _) = "<lambda>" 
  show (LValBif _) = "<builtin>" 
  show (LValStruct s fields) = "struct " ++ s ++ " {" ++ unwords (map (\(f, v) -> f ++ " = " ++ show v) fields) ++ "}"

type VEnv = Env String LVal

type LBif = LVal -> LResult LVal 

instance Eq LVal where 
  (LValBool b1) == (LValBool b2) = b1 == b2 
  (LValInt i1) == (LValInt i2) = i1 == i2 
  LValUnit == LValUnit = True 
  (LValString s1) == (LValString s2) = s1 == s2 
  (LValLam _ _ _) == (LValLam _ _ _) = False 
  (LValBif _) == (LValBif _) = False 
  _ == _ = False