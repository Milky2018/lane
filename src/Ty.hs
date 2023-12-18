{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Ty (LType (..), LTypeVal (..), pretty, UDT, lookupUdt) where
import Pretty (Pretty (pretty))
import Env
import Data.List (intercalate)

data LType = 
    LTLam LType LType
  | LTId String 
  deriving (Show, Eq) 

type UDT = Env String LTypeVal

data LTypeVal = 
    TVInt 
  | TVString
  | TVBool
  | TVUnit
  | TVStruct String [(String, LTypeVal)]
  | TVEnum String [(String, [LTypeVal])]
  | TVLam LTypeVal LTypeVal
  deriving (Show, Eq)

lookupUdt :: LType -> UDT -> LTypeVal
lookupUdt (LTId name) udt = case lookupEnv name udt of 
  Nothing -> error $ "Type " ++ name ++ " not found"
  Just t -> t
lookupUdt (LTLam t1 t2) udt = TVLam (lookupUdt t1 udt) (lookupUdt t2 udt)

instance Pretty LType where 
  pretty (LTLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"
  pretty (LTId name) = name

instance Pretty LTypeVal where 
  pretty TVInt = "Int"
  pretty TVString = "String"
  pretty TVBool = "Bool"
  pretty TVUnit = "()"
  pretty (TVStruct name fields) = name ++ " {" ++ intercalate ", " (map (\(f, t) -> f ++ " : " ++ pretty t) fields) ++ "}"
  -- TODO: in the future we will support enum definitions like this:
  --   enum OptionInt { some : Int -> OptionInt, None : OptionInt }
  -- pretty (TVEnum name variants) = name ++ " {" ++ intercalate ", " (map (\(f, ts) -> f ++ " : " ++ intercalate " -> " (map pretty ts)) variants) ++ "}"
  pretty (TVEnum name variants) = name ++ " {" ++ intercalate ", " (fmap prettyVaraint variants) ++ "}"
    where 
      prettyVaraint (f, ts) = f ++ "[" ++ intercalate ", " (fmap pretty ts) ++ "]"
  pretty (TVLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"