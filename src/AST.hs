module AST (Expr (..), LExpr, pretty, TLStmt (..), Prog (..), LTLStmt, LProg) where 

newtype Prog t = Prog [TLStmt t] deriving (Show, Eq)

data TLStmt t 
  = TLExp String t (Expr t) 
  deriving (Show, Eq)

data Expr t = 
    EBool Bool 
  | EInt Int 
  | EUnit
  | EString String
  | EId String
  | EApp (Expr t) (Expr t)
  | ELam String t (Expr t) t
  | EIf (Expr t) (Expr t) (Expr t)
  deriving (Eq, Show)

type LProg = Prog ()
type LExpr = Expr ()
type LTLStmt = TLStmt ()

pretty :: LProg -> String 
pretty (Prog stmts) = unlines $ map prettyStmt stmts

prettyStmt :: LTLStmt -> String
prettyStmt (TLExp name _ body) = name ++ " = " ++ prettyExpr body

prettyExpr :: LExpr -> String 
prettyExpr (EBool b) = show b 
prettyExpr (EId i) = i 
prettyExpr EUnit = "()" 
prettyExpr (EString s) = show s
prettyExpr (EInt i) = show i
prettyExpr (EApp e1 e2) = "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (ELam arg _ body _) = "(\\" ++ arg ++ " -> " ++ prettyExpr body ++ ")"
prettyExpr (EIf cond b1 b2) = "(if " ++ prettyExpr cond ++ " then " ++ prettyExpr b1 ++ " else " ++ prettyExpr b2 ++ ")"
