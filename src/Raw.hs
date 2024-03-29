module Raw (RExpr (..), RType (..), trans, RProg (..), TypedName (..), RTLStmt (..), RBranch (..)) where

import AST
  ( Expr (..),
    Prog (..),
    TLStmt (..), EBranch (..),
  )
import qualified Data.Bifunctor
import TAST (MTProg, MTStmt)
import Ty (LType (..))

-- newtype RProg = RProg RExpr deriving (Show, Eq)
newtype RProg = RProg [RTLStmt] deriving (Show, Eq)

data RTLStmt
  = RTLFunc String [TypedName] RExpr (Maybe RType) -- def f (x1 : t1) (x2 : t2) ... = expr
  | RTLExp TypedName RExpr -- def x : t = expr
  | RTLEnum String [(String, [RType])] -- enum E { C1[t11, t12, ... ], C2[t21, t22, ... ], ... }
  deriving (Show, Eq)

data RExpr
  = REInt Int -- 1 | 2 | 3 | ...
  | REString String -- ""
  | REId String -- id
  | RELet [(TypedName, RExpr)] RExpr -- let x1 : t1 = e1, x2 : t2 = e2 ... in expr
  | RELetrec [(TypedName, RExpr)] RExpr -- letrec f1 : t1 = e1, f2 : t2 = e2 ... in expr
  | REBin String RExpr RExpr -- expr + expr
  | RELam [TypedName] RExpr (Maybe RType) -- fn (x1 : t1) (x2 : t2) ... \=> expr
  | REIf RExpr RExpr RExpr -- if expr then expr else expr
  | REMatch RExpr [RBranch] -- match expr { C1 x1 x2 ... \=> expr, C2 x1 x2 ... \=> expr, ... }
  deriving (Show, Eq)

data RBranch = RBranch String [String] RExpr deriving (Show, Eq)

data RType
  = RTFunc RType RType
  | RTId String
  deriving (Show, Eq)

data TypedName = TypedName String (Maybe RType) deriving (Show, Eq)

trans :: RProg -> MTProg
trans (RProg re) = Prog (map transTLStmt re)
  where
    transTLStmt :: RTLStmt -> MTStmt
    transTLStmt (RTLExp (TypedName name ty) body) =
      TLExp
        name
        (fmap transType ty)
        (transExpr body)
    transTLStmt (RTLFunc name args body ty) =
      TLExp
        name
        (transType <$> combineTypes (map (\(TypedName _ ty') -> ty') args) ty)
        (transExpr (RELam args body ty))
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
    transExpr (REBin " " e1 e2) = EApp (transExpr e1) (transExpr e2)
    transExpr (REBin op e1 e2) = EApp (EApp (EId op) (transExpr e1)) (transExpr e2)
    -- fn (x1 : t1) (x2 : t2) : rt => body
    -- fn (x1 : t) : ? => fn (x2 : t2) : rt => body
    transExpr (RELam args e retT) = case args of
      [] -> error "compiler error: RELam with empty argument list"
      [TypedName var ty] -> ELam var (fmap transType ty) (transExpr e) (transType <$> retT)
      (TypedName var ty) : rest -> ELam var (fmap transType ty) (transExpr (RELam rest e Nothing)) (transType <$> Nothing)
    transExpr (REIf cond b1 b2) = EIf (transExpr cond) (transExpr b1) (transExpr b2)
    -- match e0 { pat1 => e1, pat2 => e2 }
    transExpr (REMatch e0 branches) = EMatch (transExpr e0) (map transBranch branches)

    transType (RTFunc t1 t2) = LTLam (transType t1) (transType t2)
    transType (RTId i) = LTId i

    transBranch (RBranch cons args body) = EBranch cons args (transExpr body)
