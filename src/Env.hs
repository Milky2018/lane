module Env where 

import Data.Map

type Env k v = Map k v 

emptyEnv :: Env k v  
emptyEnv = Data.Map.empty

envLookup :: Ord k => k -> Env k v -> Maybe v 
envLookup = Data.Map.lookup

extendEnv :: Ord k => k -> v -> Env k v -> Env k v 
extendEnv = Data.Map.insert