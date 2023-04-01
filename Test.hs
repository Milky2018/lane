class F a where 
  f :: a -> a 
  f = helper 

helper :: F a => a -> a 
helper = f

instance F Int where 
  f = helper 