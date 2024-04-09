module AST (Expr (..), LExpr, TLStmt (..), Prog (..), LTLStmt, LProg, transProg, EBranch (..)) where

import qualified Data.Bifunctor
import Prettyprinter
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty (map)

newtype Prog t = Prog [TLStmt t] deriving (Eq)

data TLStmt t
  = TLExp String t (Expr t)
  | TLEnum String [(String, [t])]
  deriving (Eq)

data Expr t
  = EInt Int
  | EString String
  | EId String
  | EApp (Expr t) (Expr t)
  | ELam String t (Expr t) t
  | ETypeLam String (Expr t)
  | ELetrec [(String, t, Expr t)] (Expr t)
  | EIf (Expr t) (Expr t) (Expr t)
  | EMatch (Expr t) (NonEmpty (EBranch t))
  | ETypeApp (Expr t) t 
  deriving (Eq)

data EBranch t = EBranch String [String] (Expr t) deriving (Eq)

type LProg = Prog ()

type LExpr = Expr ()

type LTLStmt = TLStmt ()

transProg :: (a -> b) -> Prog a -> Prog b
transProg f (Prog stmts) = Prog $ map (transStmt f) stmts

transStmt :: (a -> b) -> TLStmt a -> TLStmt b
transStmt f (TLExp name ty body) = TLExp name (f ty) (transExpr f body)
transStmt f (TLEnum enum variants) = TLEnum enum (map (Data.Bifunctor.second (map f)) variants)

transExpr :: (a -> b) -> Expr a -> Expr b
transExpr _f (EInt i) = EInt i
transExpr _f (EString s) = EString s
transExpr _f (EId i) = EId i
transExpr f (EApp e1 e2) = EApp (transExpr f e1) (transExpr f e2)
transExpr f (ELam name ty body retTy) = ELam name (f ty) (transExpr f body) (f retTy)
transExpr f (ETypeLam name body) = ETypeLam name (transExpr f body)
transExpr f (ELetrec bindings body) = ELetrec (map (\(name, ty, expr) -> (name, f ty, transExpr f expr)) bindings) (transExpr f body)
transExpr f (EIf e1 e2 e3) = EIf (transExpr f e1) (transExpr f e2) (transExpr f e3)
transExpr f (EMatch e branches) = EMatch (transExpr f e) (Data.List.NonEmpty.map (transBranch f) branches)
transExpr f (ETypeApp e ty) = ETypeApp (transExpr f e) (f ty)

transBranch :: (a -> b) -> EBranch a -> EBranch b
transBranch f (EBranch cons pats body) = EBranch cons pats (transExpr f body)

instance (Pretty t) => Pretty (Expr t) where
  pretty (EId i) = pretty i
  pretty (EString s) = pretty "\"" <> pretty s <> pretty "\""
  pretty (EInt i) = pretty i
  pretty (EApp e1 e2) = parens $ pretty e1 <+> pretty e2
  pretty (ELam arg argT body _) = 
    parens $ pretty "\\" <> parens (pretty arg <+> pretty ":" <+> pretty argT) <+> pretty "->" <+> pretty body
  pretty (ETypeLam arg body) = parens $ pretty "\\" <> angles (pretty arg) <+> pretty "->" <+> pretty body
  pretty (ELetrec bindings body) = 
    align $ 
      pretty "letrec" <+> 
      align (vsep (map (\(name, t, expr) -> pretty name <+> pretty  ":" <+> pretty t <+> pretty "=" <+> pretty expr) bindings)) 
      <> line 
      <> pretty "in" 
      <+> pretty body
  pretty (EIf cond b1 b2) =
    align $ vsep [pretty "if" <+> pretty cond, pretty "then" <+> pretty b1, pretty "else" <+> pretty b2]
  pretty (EMatch e0 branches) =
    pretty "match" <+> pretty e0 <+> pretty "{" <+> hsep (toList $ Data.List.NonEmpty.map prettyBranch branches) <+> pretty "}"
    where prettyBranch (EBranch cons args body) = pretty cons <+> hsep (map pretty args) <+> pretty "=>" <+> pretty body
  pretty (ETypeApp e ty) = pretty e <+> pretty "@" <> pretty ty

instance (Pretty t) => Pretty (Prog t) where
  pretty (Prog stmts) = vsep $ map pretty stmts

instance (Pretty t) => Pretty (TLStmt t) where
  pretty (TLExp name t body) = pretty "def" <+> pretty name <+> pretty ":" <+> pretty t <+> pretty "=" <+> pretty body
  pretty (TLEnum name variants) = pretty "enum" <+> pretty name <+> pretty "{" <> line <> (nest 4 $ vsep (map (\(f, ts) -> pretty f <+> pretty "[" <+> hsep (map pretty ts) <+> pretty "]") variants)) <> line <> pretty "}"

