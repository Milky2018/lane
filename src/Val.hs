module Val where 

import AST 
import Err 
import Env

data LVal = 
    LValBool Bool 
  | LValInt Int 
  | LValUnit
  | LValString String
  | LValLam String LExpr VEnv 
  | LValBif LBif 

type VEnv = Env String LVal

type LBif = LVal -> LResult LVal 