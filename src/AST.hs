module AST where 

newtype LProg = LProg LExpr deriving (Show, Eq)

data LExpr = 
    LExprBool Bool 
  | LExprInt Int 
  | LExprUnit
  | LExprString String
  | LExprId String
  | LExprApp LExpr LExpr
  | LExprLam String LExpr
  | LExprIf LExpr LExpr LExpr 
  deriving (Show, Eq)

type LProgPrinter = LProg -> String 

pretty :: LProgPrinter
pretty (LProg e) = prettyExpr e

prettyExpr :: LExpr -> String 
prettyExpr (LExprBool b) = show b 
prettyExpr (LExprId i) = i 
prettyExpr LExprUnit = "()" 
prettyExpr (LExprString s) = show s
prettyExpr (LExprInt i) = show i
prettyExpr (LExprApp e1 e2) = "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (LExprLam arg body) = "(\\" ++ arg ++ " -> " ++ prettyExpr body ++ ")"
prettyExpr (LExprIf cond b1 b2) = "(if " ++ prettyExpr cond ++ " then " ++ prettyExpr b1 ++ " else " ++ prettyExpr b2 ++ ")"
