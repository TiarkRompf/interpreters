{-# LANGUAGE ConstraintKinds #-}
module Examples where

import Language

fac :: Syntax p rep => p (rep Int) (rep Int)
fac = prog (\n -> app(fix (\f -> lam(\j -> 
  ifNonZero j (times_ j (app f (plus_ j (int_ (-1))))) (int_ 1)))) n)

