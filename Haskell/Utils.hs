{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module Utils where

import GHC.Exts

-------------------------------------------------
-- Some helpful definitions:
-- a ternary, functional version of if-then-else
ifNZ x tb eb = if (not (x == 0)) then tb else eb

-- A version of Applicative parametrized by a constraint
-- This allows, for example, a String compiler to use
-- Show.
class MyApp f where
  type Ctx f a :: Constraint
  pure :: Ctx f a => a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

liftA2 f = \x y -> pure f <*> x <*> y
liftA3 f = \x y z -> pure f <*> x <*> y <*> z

instance MyApp ((->) a) where
  type Ctx ((->) a) b = ()
  pure x = \_ -> x
  f <*> x = \t -> (f t) (x t)

class ZigZag rep where
  demote :: (rep a -> rep b) -> (a -> b)
