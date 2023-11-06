# Repo Overview

Root path is a stack project workspace.

* `docs` contains documentation for the project.
* `src` contains the source code for the project.
* `examples` contains example programs for the project.
* `test` contains test programs for the project.

# Stages

Five main stages:

```
parse : Text -> RProg 
trans : RProg -> MTProg 
typecheck : MTProg -> Maybe LErr
elimType : MTProg -> LProg
runProg : LProg -> LVal 
```

These functions show the overview of all stages:

```haskell
lanei :: String -> FinalVal 
lanei = runProg . parseAndTrans 

parseAndTrans :: String -> LProg
parseAndTrans e = case parseLaneProg e of
  Left err -> error (show err)
  Right prog -> let mtprog = trans prog in case typeCheck mtprog of 
    Nothing -> elimType mtprog
    Just err -> error (reportErr err)

runProg :: LProg -> FinalVal
runProg prog = 
  let env = createInitialEnv prog (addBuiltins emptyEnv)
  in case eval (EId "main") env of
      Left err -> FinalErr $ reportErr err
      Right (LValBool b) -> FinalBool b
      Right (LValInt i) -> FinalInt i
      Right LValUnit -> FinalUnit
      Right (LValString s) -> FinalString s
      Right (LValStruct s fields) -> FinalStruct s fields
      Right (LValLam _ _ _) -> FinalErr "lambda as main expression"
      Right (LValBif _) -> FinalErr "builtin function as main expression"
```

## Parse

Implemented in Parsec. The parser parses an original program text to an AST. 

## Trans

In the translation stage, first, the raw AST will be translated to an AST with possible types (`MTProg`). 

## Init

Before typechecking, the user-defined types need to be added into the type environment. 

```haskell
typeCheck :: MTProg -> Maybe LErr
typeCheck prog =
  case initialTEnv prog (addTBuiltins emptyEnv) (addBuiltinTypes emptyEnv) of
    Right (tenv, udt) -> tcProg (lookallup udt prog) tenv
    Left err -> Just err
```

## Typecheck

The typechecker will check a `MTProg` and maybe returns an type error. 

## Elim

Simply eliminate all types in a `MTProg`. 

# Types

## RProg

Raw Lane program.

Defined in `src/Raw.hs`. 

The main difference between RProg and LProg is 

* the multiple-variable **let bindings** and multiple-argument **function definitions** will be translated to the simplest one-argument form in LProg.
* the **recursive let bindings** will be translated to **fix definitions** in LProg.

### Top Level Definitions

Containing 3 top level statement definitions: 

#### Top Level Function Definition

```
def f (x1 : t1) (x2 : t2) ... (xn : tn) : t = e
```

will be translated to `LStmt`

```
def f : t1 -> t2 -> ... -> t = fn (x1 : t1) (x2 : t2) ... (xn : tn) : t = e
```

#### Top Level Expression

```
def x : t = e 
```

will be translated to 

```
def x : t = e
```

#### Top Level Structure Definition 

```
struct S { f1 : t1, f2 : t2, ... , fn : tn }
```

will be translated to 

```
struct S { f1 : t1, f2 : t2, ... , fn : tn }
```

### Expressions 

#### Literal

* Number
* String
* Identifier

#### Let Binding 

```
let x1 : t1 = e1, x2 : t2 = e2 ... in expr
```

will be translated to LExpr:

```
(fn (x1 : t1) : ? => fn (x2 : t2) : ? => expr) e1 e2
```

TODO: 

This will cause problems: in the future we may want recursive bindings in multiple `let` clauses. 

#### Recursive Let Binding

```
letrec f1 : t1 = e1, f2: t2 = e2 in expr 
```

will be translated to LExpr:

```
(fn (f1 : t1) : ? => fn (f2 : t2) : ? => expr) (fix (fn (f1 : t1) : ? => e1)) (fix (fn (f1 : t1) : ? => e2)) 
```

TODO: 

The same problem as `let` binding.

#### Binary Operations 

```
e1 + e2
```

will be translated to LExpr:

```
(+ e1) e2
```

Each binary operation will be translated to a normal function application. The function application is also treated as a binary operation ` `. That is to say, all function application will be translated to LExpr like:

`(<app> (<app> f1 a1) a2)`

In the text level, the `<app>` is just a infix space operator ` `.

#### Closure 

```
fn (x1 : t1) (x2 : t2) : rt => body
```

will be translated to LExpr:

```
fn (x1 : t) : ? => fn (x2 : t2) : rt => body
```

#### If Expression 

```
if expr1 then expr2 else expr3
```

will be translated to LExpr:

```
if expr1 then expr2 else expr3
```

#### Struct Field Access

```
expr.f
```

will be translated to LExpr:

```
expr.f
```

Note: the dot operator `.` is not a binary operator.

#### Struct Constructure Literal

```
S { f1 = e1, f2 = e2, ... }
```

will be translated to LExpr:

```
S { f1 = e1, f2 = e2, ... }
```