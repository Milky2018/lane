module Lib ( lanei ) where

import Examples (parseAndTrans)
import Eval

lanei :: String -> FinalVal 
lanei = runProg . parseAndTrans 
