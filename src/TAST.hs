module TAST where

import Data.Map

import Env
import Ty

data LTExpr =  
    LTExprInt Int 
  | LTExprString String 
  | LTExprBool Bool 
  | LTExprUnit 
  | LTExprId String
  | LTExprApp LTExpr LTExpr
  | LTExprLam String (Maybe LType) LTExpr (Maybe LType)
  | LTExprIf LTExpr LTExpr LTExpr 
  deriving (Show, Eq)

newtype LTProg = LTProg LTExpr deriving (Show, Eq)

type TEnv = Env String LType 

pretty :: LTProg -> String 
pretty (LTProg e) = prettyExpr e 

prettyExpr :: LTExpr -> String 
prettyExpr (LTExprInt i) = show i
prettyExpr (LTExprString s) = show s
prettyExpr (LTExprBool b) = show b
prettyExpr LTExprUnit = "()"
prettyExpr (LTExprId i) = i
prettyExpr (LTExprApp e1 e2) = "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (LTExprLam arg argT body bodyT) = "(\\(" ++ arg ++ " : " ++ Ty.prettyMaybe argT ++ ") : " ++ Ty.prettyMaybe bodyT ++ " -> " ++ prettyExpr body ++ ")"
prettyExpr (LTExprIf cond b1 b2) = "(if " ++ prettyExpr cond ++ " then " ++ prettyExpr b1 ++ " else " ++ prettyExpr b2 ++ ")"
