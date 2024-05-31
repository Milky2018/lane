module Kind (calcKind) where 
import Ty
import Env
import Err

calcKind :: LCon -> Univ -> LResult LKind
calcKind LCArr _ = return $ LKArr LKType (LKArr LKType LKType)
calcKind (LCId name) udt = case lookupEnv name udt of 
  Just k -> return k
  Nothing -> Left $ LVariableNotInScope name 
calcKind (LCAll _u _k _c) _udt = return LKType 
calcKind (LCVar name) udt = case lookupEnv name udt of 
  Just k -> return k
  Nothing -> Left $ LVariableNotInScope name 
calcKind (LCApp c1 c2) udt = do 
  k1 <- calcKind c1 udt  
  k2 <- calcKind c2 udt
  case k1 of 
    LKArr k1' k2' -> do 
      if k1' == k2 then return k2' else
        Left $ LKindMismatch k1' k2
    _ -> Left $ LTypeAppOnNonForall c1 
calcKind (LCLam a c) udt = 
  let newUniv = extendEnv a LKType udt 
  in LKArr <$> calcKind (LCId a) udt <*> calcKind c newUniv