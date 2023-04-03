module Env (emptyEnv, lookupEnv, extendEnv, Env) where

import Data.Map.Lazy as Map 

type Env k v = Map k v 

emptyEnv :: Env k v  
emptyEnv = Map.empty

lookupEnv :: Ord k => k -> Env k v -> Maybe v 
lookupEnv = Map.lookup

extendEnv :: Ord k => k -> v -> Env k v -> Env k v 
extendEnv = Map.insert