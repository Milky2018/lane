module Raw where
  
import AST 
import TAST
import Ty

newtype RProg = RProg RExpr deriving (Show, Eq)

data RExpr = 
    RExprBool Bool 
  | RExprInt Int 
  | RExprUnit
  | RExprString String
  | RExprId String
  | RExprLet [(TypedName, RExpr)] RExpr
  | RExprBin String RExpr RExpr 
  | RExprLam [TypedName] RExpr (Maybe RType)
  | RExprIf RExpr RExpr RExpr 
  deriving (Show, Eq)

data RType = 
    RTBool 
  | RTUnit 
  | RTInt 
  | RTString 
  | RTFunc RType RType 
  deriving (Show, Eq)

data TypedName = TypedName String (Maybe RType) deriving (Show, Eq)

trans :: RProg -> LTProg
trans (RProg re) = LTProg (te re) where 
  te (RExprBool b) = LTExprBool b
  te (RExprInt i) = LTExprInt i
  te RExprUnit = LTExprUnit
  te (RExprString s) = LTExprString s
  te (RExprId i) = LTExprId i

  te (RExprLet bindings e) = case bindings of 
    [] -> error "compiler error: RExprLet with empty let clause"
    [(TypedName var ty, body)] -> LTExprApp (LTExprLam var (fmap tt ty) (te e) Nothing) (te body)
    ((TypedName var ty, body):rest) -> LTExprApp (LTExprLam var (fmap tt ty) (te (RExprLet rest e)) Nothing) (te body)

  te (RExprBin " " e1 e2) = LTExprApp (te e1) (te e2)
  te (RExprBin op e1 e2) = LTExprApp (LTExprApp (LTExprId op) (te e1)) (te e2)

  te (RExprLam args e retT) = case args of 
    [] -> error "compiler error: RExprLam with empty argument list"
    [TypedName var ty] -> LTExprLam var (fmap tt ty) (te e) (tt <$> retT)
    (TypedName var ty):rest -> LTExprLam var (fmap tt ty) (te (RExprLam rest e Nothing)) (tt <$> Nothing)

  te (RExprIf cond b1 b2) = LTExprIf (te cond) (te b1) (te b2)

  tt RTBool = LTypeBool
  tt RTUnit = LTypeUnit
  tt RTInt = LTypeInt
  tt RTString = LTypeString
  tt (RTFunc t1 t2) = LTypeLam (tt t1) (tt t2)
