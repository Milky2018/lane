module TC where 
import AST
import Builtins
import Env
import Err
import TAST
import Ty

typeCheck :: LTProg -> Maybe LErr 
typeCheck (LTProg e) = case tc e (addTBuiltins emptyEnv) of 
  Left err -> Just err 
  Right ty -> Nothing

tc :: LTExpr -> TEnv -> LResult LType 
tc (LTExprInt _) _ = pure LTypeInt 
tc (LTExprBool _) _ = pure LTypeBool
tc LTExprUnit _ = pure LTypeUnit 
tc (LTExprString _) _ = pure LTypeString 
tc (LTExprId var) env = case envLookup var env of 
  Nothing -> Left $ LErr $  "unbound variable: " ++ var
  Just ty -> Right ty
tc (LTExprIf cond then_ else_) env = do 
  condTy <- tc cond env 
  thenTy <- tc then_ env 
  elseTy <- tc else_ env 
  if condTy == LTypeBool && thenTy == elseTy 
    then Right thenTy 
    else Left $ LErr "type error in if expression"
tc (LTExprLam var ty body retTy) env =  
  -- TODO: we need TYPE INFERRENCE here 
  case ty of 
    Nothing -> Left $ LErr "type inferrence has not been implemented, please provide type annotations"
    Just ty -> do 
      bodyTy <- tc body (extendEnv var ty env)
      case retTy of 
        Nothing -> Right $ LTypeLam ty bodyTy
        Just retTy -> if retTy == bodyTy 
          then Right $ LTypeLam ty bodyTy 
          else Left $ LTypeErr body retTy bodyTy
tc (LTExprApp f arg) env = do 
  fTy <- tc f env 
  argTy <- tc arg env 
  case fTy of 
    LTypeLam argTy' retTy -> if argTy == argTy' 
      then Right retTy 
      else Left $ LTypeErr arg argTy argTy'
    _ -> Left $ LErr $ "type error in application: " ++ show f ++ " is not a function"

elimType :: LTProg -> LProg
elimType (LTProg e) = LProg $ elimTypeExpr e
  where elimTypeExpr :: LTExpr -> LExpr
        elimTypeExpr (LTExprInt i) = LExprInt i
        elimTypeExpr (LTExprString s) = LExprString s
        elimTypeExpr (LTExprBool b) = LExprBool b
        elimTypeExpr LTExprUnit = LExprUnit
        elimTypeExpr (LTExprId i) = LExprId i
        elimTypeExpr (LTExprApp e1 e2) = LExprApp (elimTypeExpr e1) (elimTypeExpr e2)
        elimTypeExpr (LTExprLam arg _ body _) = LExprLam arg (elimTypeExpr body)
        elimTypeExpr (LTExprIf cond b1 b2) = LExprIf (elimTypeExpr cond) (elimTypeExpr b1) (elimTypeExpr b2)