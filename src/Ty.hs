{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Ty (LType (..), pretty, UDT) where

import Prettyprinter

data LType = 
    LTLam LType LType
  | LTId String 
  | LTAll String LType 
  | LTVar String 

instance Eq LType where 
  LTLam t1 t2 == LTLam t1' t2' = t1 == t1' && t2 == t2'
  LTId name == LTId name' = name == name'
  LTAll n ty == LTAll n' ty' = 
    ty == subst ty' n' n 
    where 
      subst :: LType -> String -> String -> LType 
      subst (LTId name) name' t = if name == name' then LTId t else LTId name
      subst (LTLam t1 t2) name' t = LTLam (subst t1 name' t) (subst t2 name' t)
      subst (LTAll name'' t) name' t' = if name'' == name' then LTAll name'' t else LTAll name'' (subst t name' t')
      subst (LTVar v) _ _ = error $ "There should not be type variable " ++ v
  LTVar v == _ = error $ "There should not be type variable " ++ v
  _ == LTVar v = error $ "There should not be type variable " ++ v
  _ == _ = False

type UDT = [String]

instance Pretty LType where 
  pretty (LTLam t1 t2) = parens $ pretty t1 <+> pretty "->" <+> pretty t2
  pretty (LTId name) = pretty name
  pretty (LTAll name t) = parens $ angles (pretty name) <+> pretty t
  pretty (LTVar name) = pretty name
