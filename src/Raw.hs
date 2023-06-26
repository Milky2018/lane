module Raw (RExpr (..), RType (..), trans, RProg (..), TypedName (..), RTLStmt (..)) where

import AST
  ( Expr (..), Prog (..), TLStmt (..),
  )
import TAST (MTProg, MTStmt)
import Ty (LType (..))
import qualified Data.Bifunctor

-- newtype RProg = RProg RExpr deriving (Show, Eq)
newtype RProg = RProg [RTLStmt] deriving (Show, Eq)

data RTLStmt
  = RTLFunc String [TypedName] RExpr (Maybe RType) -- def f (x1 : t1) (x2 : t2) ... \=> expr
  | RTLExp TypedName RExpr -- def x : t \=> expr
  | RTLStruct String [TypedName] -- struct S { field1 : t1, field2 : t2, ... }
  deriving (Show, Eq)

data RExpr
  = REInt Int -- 1 | 2 | 3 | ...
  | REString String -- ""
  | REId String -- id
  | RELet [(TypedName, RExpr)] RExpr -- let x1 : t1 = e1; x2 : t2 = e2 ... in expr
  | REBin String RExpr RExpr -- expr + expr
  | RELam [TypedName] RExpr (Maybe RType) -- fun (x1 : t1) (x2 : t2) ... \=> expr
  | REIf RExpr RExpr RExpr -- if expr then expr else expr
  | REAccess RExpr String -- expr . field
  | REStructCons String [(String, RExpr)] -- S { field1 = e1, field2 = e2, ... }
  deriving (Show, Eq)

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
    transTLStmt (RTLStruct name fields) =
      TLStruct
        name
        (map (\(TypedName name' ty) -> (name', transType <$> ty)) fields)

    combineTypes :: [Maybe RType] -> Maybe RType -> Maybe RType
    combineTypes [] rt = rt
    combineTypes (x:xs) rt = do
      x' <- x
      xs' <- combineTypes xs rt
      pure $ RTFunc x' xs'

    transExpr (REInt i) = EInt i
    transExpr (REString s) = EString s
    transExpr (REId i) = EId i
    -- let x1 : t1 = e1; x2 : t2 = e2 in expr
    -- (fun (x1 : t1) : ? => fun (x2 : t2) : ? => expr) e1 e2
    transExpr (RELet bindings e) = case bindings of
      [] -> error "compiler error: RELet with empty let clause"
      [(TypedName var ty, body)] -> EApp (ELam var (fmap transType ty) (transExpr e) Nothing) (transExpr body)
      ((TypedName var ty, body) : rest) -> EApp (ELam var (fmap transType ty) (transExpr (RELet rest e)) Nothing) (transExpr body)
    transExpr (REBin " " e1 e2) = EApp (transExpr e1) (transExpr e2)
    transExpr (REBin op e1 e2) = EApp (EApp (EId op) (transExpr e1)) (transExpr e2)
    -- fun (x1 : t1) (x2 : t2) : rt => body
    -- fun (x1 : t) : ? => fun (x2 : t2) : rt => body
    transExpr (RELam args e retT) = case args of
      [] -> error "compiler error: RELam with empty argument list"
      [TypedName var ty] -> ELam var (fmap transType ty) (transExpr e) (transType <$> retT)
      (TypedName var ty) : rest -> ELam var (fmap transType ty) (transExpr (RELam rest e Nothing)) (transType <$> Nothing)
    transExpr (REIf cond b1 b2) = EIf (transExpr cond) (transExpr b1) (transExpr b2)
    transExpr (REAccess e field) = EAccess (transExpr e) field
    transExpr (REStructCons name fields) = EStruct name (map (Data.Bifunctor.second transExpr) fields)

    transType (RTFunc t1 t2) = LTLam (transType t1) (transType t2)
    transType (RTId i) = LTId i
