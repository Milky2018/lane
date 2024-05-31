{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Ty (LCon (..), pretty, Univ, LKind (..), typeArr, typeForall, subst, conApp) where

import Prettyprinter
import Env

data LKind = 
    LKType
  | LKArr LKind LKind 
  deriving (Eq, Show)

data LCon = 
    LCArr -- ->
  | LCId String -- ti 
  | LCAll String LKind LCon -- <A>; Now only support kind *
  | LCVar String -- tv
  | LCApp LCon LCon -- c1[c2]
  | LCLam String LCon -- \A.c 

subst :: String -> LCon -> LCon -> LCon
subst _ _ LCArr = LCArr 
subst arg t (LCId x) = if x == arg then t else LCId x 
subst arg t (LCAll u k c) = if u == arg then LCAll u k c else LCAll u k (subst arg t c)  
subst arg t (LCApp c1 c2) = LCApp (subst arg t c1) (subst arg t c2)
subst arg t (LCLam a c) = if a == arg then LCLam a c else LCLam a (subst arg t c)
subst _ _ (LCVar _v) = error "There should not be type variable"

conApp :: LCon -> LCon -> LCon 
conApp (LCAll u _k c) c2 = subst u c2 c
conApp _c1 _c2 = error "There should not be conApp"

instance Eq LCon where
  LCArr == LCArr = True 
  LCId name == LCId name' = name == name'
  LCAll u k c == LCAll u' k' c' =
    let c'' = subst u' (LCId u) c'
    in k == k' && c == c''
  LCVar name == LCVar name' = name == name'
  LCApp c1 c2 == LCApp c1' c2' = c1 == c1' && c2 == c2'
  LCLam a c == LCLam a' c' =  
    let c'' = subst a' (LCId a) c'
    in c == c''
  _ == _ = False

typeArr :: LCon -> LCon -> LCon 
typeArr t1 t2 = LCApp (LCApp LCArr t1) t2

typeForall :: String -> LCon -> LCon
typeForall u c = LCAll u LKType c 

type Univ = Env String LKind  

instance Pretty LKind where 
  pretty LKType = pretty "*"
  pretty (LKArr k1 k2) = pretty k1 <+> pretty "->" <+> pretty k2

instance Pretty LCon where 
  pretty LCArr = pretty "(->)"
  pretty (LCId name) = pretty name
  pretty (LCAll u k c) = pretty "forall" <+> pretty u <+> pretty "::" <+> pretty k <> pretty "." <+> pretty c 
  pretty (LCVar name) = pretty name
  pretty (LCApp c1 c2) = parens (pretty c1 <+> pretty c2)
  pretty (LCLam a c) = pretty "\\" <> pretty a <> pretty "." <+> pretty c

