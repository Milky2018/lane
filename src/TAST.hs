module TAST (MTExpr, MTStmt, MTProg, TEnv) where

import Ty ( LType )
import AST (Expr (..), TLStmt (..), Prog (..))
import Data.Map (Map)

type MTExpr = Expr (Maybe LType)
type MTStmt = TLStmt (Maybe LType)
type MTProg = Prog (Maybe LType)

type TEnv = Map String LType