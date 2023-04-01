module TAST (TEnv, MTExpr, MTStmt, MTProg) where

import Env ( Env )
import Ty ( LType )
import AST (Expr (..), TLStmt (..), Prog (..))

type MTExpr = Expr (Maybe LType)
type MTStmt = TLStmt (Maybe LType)
type MTProg = Prog (Maybe LType)

type TEnv = Env String LType 

