module Lib
    ( someFunc
    ) where

import Examples (showExamples)

someFunc :: IO ()
someFunc = showExamples
