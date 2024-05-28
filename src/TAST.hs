module TAST (MTExpr, MTStmt, MTProg, TEnv) where

import Ty ( LCon )
import AST (Expr (..), TLStmt (..), Prog (..))
import Data.Map (Map)

type MTExpr = Expr (Maybe LCon)
type MTStmt = TLStmt (Maybe LCon)
type MTProg = Prog (Maybe LCon)

type TEnv = Map String LCon