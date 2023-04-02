module AST (Expr (..), LExpr, TLStmt (..), Prog (..), LTLStmt, LProg) where 
import Pretty (Pretty (pretty))

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

instance Pretty t => Pretty (Expr t) where 
  pretty = prettyExpr

instance Pretty t => Pretty (Prog t) where
  pretty = prettyProg 

instance Pretty t => Pretty (TLStmt t) where
  pretty = prettyStmt

prettyProg :: Pretty t => Prog t -> String 
prettyProg (Prog stmts) = unlines $ map prettyStmt stmts

prettyStmt :: Pretty t => TLStmt t -> String
prettyStmt (TLExp name _ body) = name ++ " = " ++ prettyExpr body

prettyExpr :: Pretty t => Expr t -> String 
prettyExpr (EBool b) = show b 
prettyExpr (EId i) = i 
prettyExpr EUnit = "()" 
prettyExpr (EString s) = show s
prettyExpr (EInt i) = show i
prettyExpr (EApp e1 e2) = "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (ELam arg argT body _) = "(\\" ++ "(" ++ arg ++ " : " ++ pretty argT ++ ") -> " ++ prettyExpr body ++ ")"
prettyExpr (EIf cond b1 b2) = "(if " ++ prettyExpr cond ++ " then " ++ prettyExpr b1 ++ " else " ++ prettyExpr b2 ++ ")"
