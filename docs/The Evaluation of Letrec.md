# Introduction

Letrec is a special primitive. Since now v0.1.3, the **letrec** expression has the following semantics:

```
def main : Int = 
  letrec fact : Int -> Int = fn x => 
    if x == 0 then 1 else x * fact (x - 1)
  in fact 5
```

This program evaluates to 120. 

But wait, the evaluation for this expression is not straightforward. Before we talk about **letrec**, I want to introduce what else does Lane do for recursion.

# Mutually dependent top level definitions

The top level function definitions can be mutually dependent. For example:

```
def odd (n : Int) : Bool =
    if n == 0 
    then false 
    else even (n - 1)
    
def even (n : Int) : Bool =
    if n == 0 
    then true 
    else odd (n - 1)
    
def main : Bool = odd 5
```

This program compiles and will work correctly. The logic that processes these top level definitions are here:

```haskell
createInitialEnv :: LProg -> VEnv -> VEnv
createInitialEnv (Prog defs) oldEnv = foldl addDef oldEnv defs
  where
    addDef env stmt@(TLExp name _ expr) = extendEnv name (evalTopLevelExpr newEnv expr) env
    addDef env stmt@(TLStruct _ _) = env 
    newEnv = createInitialEnv (Prog defs) oldEnv
```

As you can see, `addDef` will firstly evaluate the expression in the top level definition, and then add the value to the environment. The `newEnv` is the environment that contains all the top level definitions. This is a recursive call. These definitions will not make infinite loop because of Haskell's lazy evaluation. 

We can also achieve this by saving the arguments (`newEnv` and `expr` here) in a data structure and not evaluating them until they are needed. And that is just what is done in EOPL 3.4. This approach is more general but not necessary in Haskell. 

Another approach is given in SICP 4.1.6 and EOPL Exercise 3.35, 3.36. This approach is more general and can be used in any language. It is also more efficient than the previous approach. This implementation uses mutatable variables in Scheme, which is though possible in Haskell, but very hard to implement. 

For now, this only works for top level definitions in Lane. Ideally, we want to make this work for all multiple bindings. To make this clear, we refer to the four different let bindings in Scheme (R6RS): `let`, `let*`, `letrec` and `letrec*`. The top level definitions in Lane is just like `letrec*`. Next, see how we implement `letrec` and what problem remains.

# Letrec bindings

Lane allows the typical **letrec** binding shown in the introduction. Generally, the implementations for **letrec** and **multiple bindings** are just the same. First, let's look at the implementation for **letrec** in Lane:

```haskell
eval (EFix arg _ body _) env = mfix $ \val -> do
  let newEnv = extendEnv arg val env
  eval body newEnv
```

We can see that the letrec expression will firstly translated to a fix expression. Here a monad fix operator is used to implement the recursive call. Since Haskell does not support variable mutation directly, we have to use its lazy evaluation to perform the recursive call. 

The core of this code sheet is just like the `createInitialEnv` function. I think it is clear now. I won't further explain it. 

# Multiple bindings

Of course Lane does support multiple bindings. But the big question is that, Lane does not support sequential multiple bindings. For example, the following code will not compile:

```
def main : Int = 
  let x = y
    , y = 5
  in x
```

This will raise an error: 

```
Unbound variable :y
```

Sometimes we do not think this feature is necessary or even good. So we will not implement it in Lane.

# Multiple letrec bindings

But, considering that we have implemented mutually dependent top level definitions, there is no reason to not support **multiple letrec**. Like this:

```
def main : Int = 
  letrec odd = ...
       , even = ... 
  in odd 5
```

The problem for Lane v0.1.3 is that the `letrec` expressions will be translated to `fix` operations recursively by each binding.

The solution is that, in the future, we shall modify the translation and interpretation of `letrec` expressions. 