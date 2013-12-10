{-# LANGUAGE ConstraintKinds #-}
module Examples where

import Language

fac :: Syntax rep => () -> rep (rep Int -> rep Int)
fac () = lam (\n -> app(fix (\f -> lam(\j -> 
  ifNonZero j (times_ j (app f (plus_ j (int_ (-1))))) (int_ 1)))) n)

