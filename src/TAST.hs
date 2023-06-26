module TAST (MTExpr, MTStmt, MTProg, TVEnv, TVExpr, TVStmt, TVProg, lookallup ) where

import Env ( Env )
import Ty ( LType, LTypeVal, UDT, lookupUdt )
import AST (Expr (..), TLStmt (..), Prog (..))
import qualified Data.Bifunctor

type MTExpr = Expr (Maybe LType)
type MTStmt = TLStmt (Maybe LType)
type MTProg = Prog (Maybe LType)

-- type TEnv = Env String LType

type TVEnv = Env String LTypeVal

type TVExpr = Expr (Maybe LTypeVal)
type TVStmt = TLStmt (Maybe LTypeVal)
type TVProg = Prog (Maybe LTypeVal)

lookallup :: UDT -> MTProg -> TVProg
lookallup udt (Prog stmts) = Prog $ map (lookallupStmt udt) stmts

lookallupStmt :: UDT -> MTStmt -> TVStmt
lookallupStmt udt (TLExp name ty body) = TLExp name (fmap (`lookupUdt` udt) ty) (lookallupExpr udt body)
lookallupStmt udt (TLStruct struct fields) = TLStruct struct (map (Data.Bifunctor.second (fmap (`lookupUdt` udt))) fields)

lookallupExpr :: UDT -> MTExpr -> TVExpr
lookallupExpr _udt (EInt i) = EInt i
lookallupExpr _udt (EString s) = EString s
lookallupExpr _udt (EId s) = EId s
lookallupExpr udt (EApp e1 e2) = EApp (lookallupExpr udt e1) (lookallupExpr udt e2)
lookallupExpr udt (ELam name ty body retTy) = ELam name (fmap (`lookupUdt` udt) ty) (lookallupExpr udt body) (fmap (`lookupUdt` udt) retTy)
lookallupExpr udt (EIf e1 e2 e3) = EIf (lookallupExpr udt e1) (lookallupExpr udt e2) (lookallupExpr udt e3)
lookallupExpr udt (EAccess e field) = EAccess (lookallupExpr udt e) field
lookallupExpr udt (EStruct name fields) = EStruct name (map (Data.Bifunctor.second (lookallupExpr udt)) fields)
