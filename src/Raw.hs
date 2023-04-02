module Raw (RExpr (..), RType (..), trans, RProg (..), TypedName (..), RTLStmt (..)) where

import AST
  ( Expr (EApp, EBool, EId, EIf, EInt, ELam, EString, EUnit), Prog (..), TLStmt (..),
  )
import TAST (MTProg, MTStmt)
import Ty (LType (..))

-- newtype RProg = RProg RExpr deriving (Show, Eq)
newtype RProg = RProg [RTLStmt] deriving (Show, Eq)

data RTLStmt 
  = RTLFunc String [TypedName] RExpr (Maybe RType) -- def f (x1 : t1) (x2 : t2) ... \=> expr
  | RTLExp TypedName RExpr -- def x : t \=> expr
  deriving (Show, Eq)

data RExpr
  = REBool Bool -- true | false
  | REInt Int -- 1 | 2 | 3 | ...
  | REUnit -- unit
  | REString String -- ""
  | REId String -- id
  | RELet [(TypedName, RExpr)] RExpr -- let x1 : t1 = e1; x2 : t2 = e2 ... in expr
  | REBin String RExpr RExpr -- expr + expr
  | RELam [TypedName] RExpr (Maybe RType) -- fun (x1 : t1) (x2 : t2) ... \=> expr
  | REIf RExpr RExpr RExpr -- if expr then expr else expr
  deriving (Show, Eq)

data RType
  = RTBool
  | RTUnit
  | RTInt
  | RTString
  | RTFunc RType RType
  deriving (Show, Eq)

data TypedName = TypedName String (Maybe RType) deriving (Show, Eq)

trans :: RProg -> MTProg
trans (RProg re) = Prog (map transTLStmt re)
  where
    transTLStmt :: RTLStmt -> MTStmt 
    transTLStmt (RTLFunc _name [] _body _ty) = error "compiler error: RTLFunc with empty argument list"
    transTLStmt (RTLFunc name args body ty) = TLExp name (fmap transType ty) (transExpr (RELam args body ty))
    transTLStmt (RTLExp (TypedName name ty) body) = TLExp name (fmap transType ty) (transExpr body)
    
    transExpr (REBool b) = EBool b
    transExpr (REInt i) = EInt i
    transExpr REUnit = EUnit
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

    transType RTBool = LTBool
    transType RTUnit = LTUnit
    transType RTInt = LTInt
    transType RTString = LTString
    transType (RTFunc t1 t2) = LTLam (transType t1) (transType t2)
