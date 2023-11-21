{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Ty (LType (..), LTypeVal (..), pretty, UDT, lookupUdt) where
import Pretty (Pretty (pretty))
import Env
import Data.List (intercalate)

data LType = 
    LTStruct String [(String, LType)]
  | LTLam LType LType
  | LTId String 
  | LTEnum String [(String, [LType])]
  deriving (Show, Eq) 

type UDT = Env String LTypeVal

data LTypeVal = 
    TVInt 
  | TVString
  | TVBool
  | TVUnit
  | TVStruct String [(String, LTypeVal)]
  | TVLam LTypeVal LTypeVal
  deriving (Show, Eq)

lookupUdt :: LType -> UDT -> LTypeVal
lookupUdt (LTId name) udt = case lookupEnv name udt of 
  Nothing -> error $ "Type " ++ name ++ " not found"
  Just t -> t
lookupUdt (LTLam t1 t2) udt = TVLam (lookupUdt t1 udt) (lookupUdt t2 udt)
lookupUdt (LTStruct name fields) udt = 
  case lookupEnv name udt of 
    Just (TVStruct _name _userDefStruct) -> 
      let fields' = map (\(f, t) -> (f, lookupUdt t udt)) fields
      in TVStruct name fields'
    _ -> error $ "Type " ++ name ++ " not found"
lookupUdt (LTEnum _name _variants) _udt = undefined

instance Pretty LType where 
  pretty (LTLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"
  pretty (LTId name) = name
  pretty (LTStruct name fields) = "struct " ++ name ++ " {" ++ intercalate ", " (map (\(f, t) -> f ++ " : " ++ pretty t) fields) ++ "}"
  pretty (LTEnum name variants) = "enum " ++ name ++ " {" ++ intercalate ", " (map (\(f, ts) -> f ++ "[ " ++ intercalate ", " (map pretty ts) ++ " ]") variants) ++ "}"

instance Pretty LTypeVal where 
  pretty TVInt = "Int"
  pretty TVString = "String"
  pretty TVBool = "Bool"
  pretty TVUnit = "()"
  pretty (TVStruct name fields) = name ++ " {" ++ intercalate ", " (map (\(f, t) -> f ++ " : " ++ pretty t) fields) ++ "}"
  pretty (TVLam t1 t2) = "(" ++ pretty t1 ++ " -> " ++ pretty t2 ++ ")"