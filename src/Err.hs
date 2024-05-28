module Err (LErr (..), LResult, pretty) where 
import Ty ( LCon, LKind )
import TAST (MTExpr)
import AST (LExpr)
import Prettyprinter
import Data.List.NonEmpty (NonEmpty, map, toList)

data LErr = 
    LTErr MTExpr LCon LCon
  | LTFiledNotFound String
  | LBug String 
  | LMultiErr (NonEmpty LErr)
  | LTopLevelDefNoAnnotation String
  | LVariableNotInScope String
  | LFunctionArgumentTypeMissing String 
  | LLetrecBindingTypeMissing String
  | LConstructorNotInScope String 
  | LPatternHasWrongNumberOfArguments String [String] 
  | LBranchesHaveDifferentTypes LCon LCon 
  | LNoPatternMatched LExpr 
  | LTypeNotInEnv LCon 
  | LTypeAppOnNonForall LCon 
  | LConstructorIsNotType String LKind
  deriving (Eq)

type LResult = Either LErr

instance Pretty LErr where 
  pretty (LTErr e t1 t2) = pretty "Type error: expression " <> pretty e <> pretty "\n\texpected " <> pretty t1 <> pretty "\n\tbut got " <> pretty t2
  pretty (LTFiledNotFound s) = pretty "field " <> pretty s <> pretty " not found"
  pretty (LBug s) = pretty "bug found: " <> pretty s
  pretty (LMultiErr errs) = vsep $ toList $ Data.List.NonEmpty.map pretty errs
  pretty (LTopLevelDefNoAnnotation name) = pretty "Top level expressions need type annotations: " <> pretty name 
  pretty (LVariableNotInScope var) = pretty "Variable not in scope: " <> pretty var
  pretty (LFunctionArgumentTypeMissing arg) = pretty "Argument is missing type annotation " <> pretty arg
  pretty (LLetrecBindingTypeMissing name) = pretty "Letrec binding is missing type annotation " <> pretty name
  pretty (LConstructorNotInScope name) = pretty "Constructor not in scope: " <> pretty name
  pretty (LPatternHasWrongNumberOfArguments cons args) = pretty "Pattern " <> pretty cons <> pretty " has wrong number of arguments: " <> pretty args
  pretty (LBranchesHaveDifferentTypes t1 t2) = pretty "Branches have different types: " <> pretty t1 <> pretty " and " <> pretty t2
  pretty (LNoPatternMatched e) = pretty "No pattern matched: " <> pretty e
  pretty (LTypeNotInEnv t) = pretty "Type not in environment: " <> pretty t
  pretty (LTypeAppOnNonForall t) = pretty "Type application on non-forall type: " <> pretty t
  pretty (LConstructorIsNotType name k) = pretty "Constructor is not a type: " <> pretty name <> pretty " with kind " <> pretty k
