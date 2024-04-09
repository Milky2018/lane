module Raw (RExpr (..), RType (..), trans, RProg (..), TypedName (..), RTLStmt (..), RBranch (..)) where

import AST
  ( Expr (..),
    Prog (..),
    TLStmt (..), EBranch (..),
  )
import qualified Data.Bifunctor
import TAST (MTProg, MTStmt)
import Ty (LType (..))
import Prettyprinter
import Data.List.NonEmpty (NonEmpty(..))

newtype RProg = RProg [RTLStmt] deriving (Eq)

instance Pretty RProg where
  pretty (RProg stmts) = vsep (map pretty stmts)

data RTLStmt
  = RTLFunc String [String] [TypedName] RExpr (Maybe RType) -- def f <A1> <A2> (x1 : t1) (x2 : t2) ... = expr
  | RTLExp TypedName RExpr -- def x : t = expr
  | RTLEnum String [(String, [RType])] -- enum E { C1[t11, t12, ... ], C2[t21, t22, ... ], ... }
  deriving (Eq)

instance Pretty RTLStmt where
  pretty (RTLFunc name typeArgs args body ty) =
    pretty "def" <+> pretty name <+> hsep (map (angles . pretty) typeArgs) <+> hsep (map pretty args) <+> pretty "=" <+> pretty body <+> maybe mempty (\t -> pretty ":" <+> pretty t) ty
  pretty (RTLExp (TypedName name ty) body) =
    pretty "def" <+> pretty name <+> pretty ":" <+> pretty ty <+> pretty "=" <+> pretty body
  pretty (RTLEnum name fields) =
    pretty "enum" <+> pretty name <+> hsep (map pretty fields)

data RExpr
  = REInt Int -- 1 | 2 | 3 | ...
  | REString String -- ""
  | REId String -- id
  | RELet [(TypedName, RExpr)] RExpr -- let x1 : t1 = e1, x2 : t2 = e2 ... in expr
  | RELetrec [(TypedName, RExpr)] RExpr -- letrec f1 : t1 = e1, f2 : t2 = e2 ... in expr
  | REBin String RExpr RExpr -- expr + expr
  | RELam [String] [TypedName] RExpr (Maybe RType) -- fn <A1> <A2> ... (x1 : t1) (x2 : t2) ... \=> expr
  | REIf RExpr RExpr RExpr -- if expr then expr else expr
  | REMatch RExpr [RBranch] -- match expr { C1 x1 x2 ... \=> expr, C2 x1 x2 ... \=> expr, ... }
  | RETypeApp RType -- @type
  deriving (Eq)

instance Pretty RExpr where 
  pretty (REInt i) = pretty i
  pretty (REString s) = pretty "\"" <> pretty s <> pretty "\""
  pretty (REId i) = pretty i
  pretty (RELet bindings e) = pretty "let" <+> hsep (map (\(TypedName name ty, body) -> pretty name <+> pretty ":" <+> pretty ty <+> pretty "=" <+> pretty body) bindings) <+> pretty "in" <+> pretty e
  pretty (RELetrec bindings e) = pretty "letrec" <+> hsep (map (\(TypedName name ty, body) -> pretty name <+> pretty ":" <+> pretty ty <+> pretty "=" <+> pretty body) bindings) <+> pretty "in" <+> pretty e
  pretty (REBin " " e1 e2) = parens $ pretty e1 <+> pretty e2
  pretty (REBin op e1 e2) = parens $ pretty e1 <+> pretty op <+> pretty e2
  pretty (RELam typeArgs args e retT) = parens $ hsep (map (angles . pretty) typeArgs) <+> pretty "\\" <> hsep (map pretty args) <+> pretty "->" <+> pretty e <+> maybe mempty (\t -> pretty ":" <+> pretty t) retT
  pretty (REIf cond b1 b2) = pretty "if" <+> pretty cond <+> pretty "then" <+> pretty b1 <+> pretty "else" <+> pretty b2
  pretty (REMatch e branches) = pretty "match" <+> pretty e <+> pretty "{" <+> hsep (punctuate comma (map pretty branches)) <+> pretty "}"
  pretty (RETypeApp ty) = pretty "@" <> pretty ty

data RBranch = RBranch String [String] RExpr deriving (Eq)

instance Pretty RBranch where 
  pretty (RBranch cons args body) = pretty cons <+> hsep (map pretty args) <+> pretty "=>" <+> pretty body

data RType
  = RTFunc RType RType
  | RTId String
  | RTAll String RType 
  deriving (Eq)

instance Pretty RType where 
  pretty (RTFunc t1 t2) = parens $ pretty t1 <+> pretty "->" <+> pretty t2
  pretty (RTId i) = pretty i
  pretty (RTAll i t) = parens $ angles (pretty i) <+> pretty t

data TypedName = TypedName String (Maybe RType) deriving (Eq)

instance Pretty TypedName where 
  pretty (TypedName name ty) = pretty name <+> maybe mempty (\t -> pretty ":" <+> pretty t) ty

trans :: RProg -> MTProg
trans (RProg re) = Prog (map transTLStmt re)
  where
    transTLStmt :: RTLStmt -> MTStmt
    transTLStmt (RTLExp (TypedName name ty) body) =
      TLExp
        name
        (fmap transType ty)
        (transExpr body)
    transTLStmt (RTLFunc name typeArgs args body ty) =
      TLExp
        name
        (transType <$> combineForallTypes typeArgs <$> combineTypes (map (\(TypedName _ ty') -> ty') args) ty)
        (transExpr (RELam typeArgs args body ty))
    transTLStmt (RTLEnum name fields) =
      TLEnum
        name
        (map (Data.Bifunctor.second (map (Just . transType))) fields)

    combineTypes :: [Maybe RType] -> Maybe RType -> Maybe RType
    combineTypes [] rt = rt
    combineTypes (x : xs) rt = do
      x' <- x
      xs' <- combineTypes xs rt
      pure $ RTFunc x' xs'

    combineForallTypes :: [String] -> RType -> RType
    combineForallTypes [] rt = rt
    combineForallTypes (x : xs) rt = RTAll x (combineForallTypes xs rt)

    transExpr (REInt i) = EInt i
    transExpr (REString s) = EString s
    transExpr (REId i) = EId i
    -- let x1 : t1 = e1, x2 : t2 = e2 in expr
    -- (fn (x1 : t1) : ? => fn (x2 : t2) : ? => expr) e1 e2
    transExpr (RELet bindings e) = case bindings of
      [] -> error "compiler error: RELet with empty let clause"
      [(TypedName var ty, body)] ->
        EApp
          (ELam var (fmap transType ty) (transExpr e) Nothing)
          (transExpr body)
      ((TypedName var ty, body) : rest) ->
        EApp
          (ELam var (fmap transType ty) (transExpr (RELet rest e)) Nothing)
          (transExpr body)
    -- letrec f1 : t1 = e1, f2 : t2 = e2 in expr
    -- letrec f1 : t1 = e1, f2 : t2 = e2 in expr
    transExpr (RELetrec bindings e) =
      ELetrec (fmap (\(TypedName name ty, body) -> (name, fmap transType ty, transExpr body)) bindings) (transExpr e)
    -- e1 + e2
    -- (+ e1) e2
    transExpr (REBin " " e1 (RETypeApp t)) = ETypeApp (transExpr e1) (Just $ transType t)
    transExpr (REBin " " e1 e2) = EApp (transExpr e1) (transExpr e2)
    transExpr (REBin op e1 e2) = EApp (EApp (EId op) (transExpr e1)) (transExpr e2)
    -- fn (x1 : t1) (x2 : t2) : rt => body
    -- fn (x1 : t) : ? => fn (x2 : t2) : rt => body
    transExpr (RELam [] args e retT) = case args of
      [] -> error "compiler error: RELam with empty argument list"
      [TypedName var ty] -> ELam var (fmap transType ty) (transExpr e) (transType <$> retT)
      (TypedName var ty) : rest -> ELam var (fmap transType ty) (transExpr (RELam [] rest e Nothing)) (transType <$> Nothing)
    transExpr (RELam (targ : targs) args e retT) = ETypeLam targ (transExpr (RELam targs args e retT))
    transExpr (REIf cond b1 b2) = EIf (transExpr cond) (transExpr b1) (transExpr b2)
    -- match e0 { pat1 => e1, pat2 => e2 }
    transExpr (REMatch _e0 []) = error "compiler error: REMatch with empty branch list"
    transExpr (REMatch e0 (b:bs)) = EMatch (transExpr e0) ((transBranch b) :| (map transBranch bs))
    transExpr (RETypeApp t) = error $ "compiler error: RETypeApp should have been handled by REBin: " ++ show (pretty t)

    transType (RTFunc t1 t2) = LTLam (transType t1) (transType t2)
    transType (RTId i) = LTId i
    transType (RTAll i t) = LTAll i (transType t)

    transBranch (RBranch cons args body) = EBranch cons args (transExpr body)
