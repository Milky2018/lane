{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Ty (LCon (..), pretty, Univ, LKind (..), calcKind, typeArr, typeForall, subst) where

import Prettyprinter
import Env

data LKind = 
    LKType
  | LKArr LKind LKind 
  deriving (Eq, Show)

data LCon = 
    LTArr -- ->
  | LTId String -- ti 
  | LTAll String LKind LCon -- <A>; Now only support kind *
  | LTVar String -- tv
  | LTApp LCon LCon -- c1[c2]
  | LTLam String LCon -- \A.c 

subst :: String -> LCon -> LCon -> LCon
subst _ _ LTArr = LTArr 
subst arg t (LTId x) = if x == arg then t else LTId x 
subst arg t (LTAll u k c) = if u == arg then LTAll u k c else LTAll u k (subst arg t c)  
subst arg t (LTApp c1 c2) = LTApp (subst arg t c1) (subst arg t c2)
subst arg t (LTLam a c) = if a == arg then LTLam a c else LTLam a (subst arg t c)
subst _ _ (LTVar _v) = error "There should not be type variable"

instance Eq LCon where
  LTArr == LTArr = True 
  LTId name == LTId name' = name == name'
  LTAll u k c == LTAll u' k' c' =
    let c'' = subst u' (LTId u) c'
    in k == k' && c == c''
  LTVar name == LTVar name' = name == name'
  LTApp c1 c2 == LTApp c1' c2' = c1 == c1' && c2 == c2'
  LTLam a c == LTLam a' c' =  
    let c'' = subst a' (LTId a) c'
    in c == c''
  _ == _ = False

typeArr :: LCon -> LCon -> LCon 
typeArr t1 t2 = LTApp (LTApp LTArr t1) t2

typeForall :: String -> LCon -> LCon
typeForall u c = LTAll u LKType c 

type Univ = Env String LKind  

instance Pretty LKind where 
  pretty LKType = pretty "*"
  pretty (LKArr k1 k2) = pretty k1 <+> pretty "->" <+> pretty k2

instance Pretty LCon where 
  pretty LTArr = pretty "->"
  pretty (LTId name) = pretty name
  pretty (LTAll u k c) = pretty "forall" <+> pretty u <+> pretty "::" <+> pretty k <> pretty "." <+> pretty c 
  pretty (LTVar name) = pretty name
  pretty (LTApp c1 c2) = pretty c1 <+> pretty c2
  pretty (LTLam a c) = pretty "\\" <> pretty a <> pretty "." <+> pretty c

calcKind :: LCon -> Univ -> LKind
calcKind LTArr _ = LKArr LKType (LKArr LKType LKType)
calcKind (LTId name) udt = case lookupEnv name udt of 
  Just k -> k
  Nothing -> error $ "Type not in environment: " ++ name ++ show udt
calcKind (LTAll _u _k _c) _udt = LKType 
calcKind (LTVar name) udt = case lookupEnv name udt of 
  Just k -> k
  Nothing -> error $ "Type variable not in environment: " ++ name
calcKind (LTApp c1 c2) udt = case calcKind c1 udt of
  LKArr k1 k2 -> if k1 == calcKind c2 udt then k2 else error $ "Kind application mismatch" ++ show (pretty "k1: " <+> pretty k1 <+> pretty "c2::" <+> pretty (calcKind c2 udt))
  _ -> error $ "Type application on non-forall type" ++ show (pretty c1 <+> pretty c2 <+> pretty (calcKind c1 udt))
calcKind (LTLam a c) udt = 
  let newUniv = extendEnv a LKType udt 
  in LKArr (calcKind (LTId a) udt) (calcKind c newUniv)