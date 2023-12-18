module AST (Expr (..), LExpr, TLStmt (..), Prog (..), LTLStmt, LProg, transProg) where

import qualified Data.Bifunctor
import Data.List (intercalate)
import Pretty (Pretty (pretty))

newtype Prog t = Prog [TLStmt t] deriving (Show, Eq)

data TLStmt t
  = TLExp String t (Expr t)
  | TLStruct String [(String, t)]
  | TLEnum String [(String, [t])]
  deriving (Show, Eq)

data Expr t
  = EInt Int
  | EString String
  | EId String
  | EApp (Expr t) (Expr t)
  | ELam String t (Expr t) t
  | ELetrec [(String, t, Expr t)] (Expr t)
  | EIf (Expr t) (Expr t) (Expr t)
  | EAccess (Expr t) String
  | EStruct String [(String, Expr t)]
  | EEnum String String [Expr t]
  deriving (Eq, Show)

type LProg = Prog ()

type LExpr = Expr ()

type LTLStmt = TLStmt ()

transProg :: (a -> b) -> Prog a -> Prog b
transProg f (Prog stmts) = Prog $ map (transStmt f) stmts

transStmt :: (a -> b) -> TLStmt a -> TLStmt b
transStmt f (TLExp name ty body) = TLExp name (f ty) (transExpr f body)
transStmt f (TLStruct struct fields) = TLStruct struct (map (Data.Bifunctor.second f) fields)
transStmt f (TLEnum enum variants) = TLEnum enum (map (Data.Bifunctor.second (map f)) variants)

transExpr :: (a -> b) -> Expr a -> Expr b
transExpr _f (EInt i) = EInt i
transExpr _f (EString s) = EString s
transExpr _f (EId i) = EId i
transExpr f (EApp e1 e2) = EApp (transExpr f e1) (transExpr f e2)
transExpr f (ELam name ty body retTy) = ELam name (f ty) (transExpr f body) (f retTy)
transExpr f (ELetrec bindings body) = ELetrec (map (\(name, ty, expr) -> (name, f ty, transExpr f expr)) bindings) (transExpr f body)
transExpr f (EIf e1 e2 e3) = EIf (transExpr f e1) (transExpr f e2) (transExpr f e3)
transExpr f (EAccess e field) = EAccess (transExpr f e) field
transExpr f (EStruct s fields) = EStruct s (map (Data.Bifunctor.second (transExpr f)) fields)
transExpr f (EEnum e c variants) = EEnum e c (map (transExpr f) variants)

instance (Pretty t) => Pretty (Expr t) where
  pretty = prettyExpr

instance (Pretty t) => Pretty (Prog t) where
  pretty = prettyProg

instance (Pretty t) => Pretty (TLStmt t) where
  pretty = prettyStmt

prettyProg :: (Pretty t) => Prog t -> String
prettyProg (Prog stmts) = unlines $ map prettyStmt stmts

prettyStmt :: (Pretty t) => TLStmt t -> String
prettyStmt (TLExp name _ body) = name ++ " = " ++ prettyExpr body
prettyStmt (TLStruct name fields) = "struct " ++ name ++ " {" ++ intercalate ", " (map (\(f, t) -> f ++ " : " ++ pretty t) fields) ++ "}"
prettyStmt (TLEnum name variants) = "enum " ++ name ++ " {" ++ intercalate ", " (map (\(f, ts) -> f ++ "[ " ++ intercalate ", " (map pretty ts) ++ " ]") variants) ++ "}"

prettyExpr :: (Pretty t) => Expr t -> String
prettyExpr (EId i) = i
prettyExpr (EString s) = show s
prettyExpr (EInt i) = show i
prettyExpr (EApp e1 e2) = "(" ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (ELam arg argT body _) = "(\\" ++ "(" ++ arg ++ " : " ++ pretty argT ++ ") -> " ++ prettyExpr body ++ ")"
prettyExpr (ELetrec bindings body) = "letrec " ++ intercalate ", " (map (\(name, t, expr) -> name ++ " : " ++ pretty t ++ " = " ++ prettyExpr expr) bindings) ++ " in " ++ prettyExpr body
prettyExpr (EIf cond b1 b2) = "(if " ++ prettyExpr cond ++ " then " ++ prettyExpr b1 ++ " else " ++ prettyExpr b2 ++ ")"
prettyExpr (EAccess e field) = prettyExpr e ++ "." ++ field
prettyExpr (EStruct name fields) = name ++ " {" ++ intercalate ", " (map (\(f, e) -> f ++ " = " ++ prettyExpr e) fields) ++ "}"
prettyExpr (EEnum enumName varName fields) = enumName ++ "." ++ varName ++ "[ " ++ intercalate ", " (map prettyExpr fields) ++ " ]"