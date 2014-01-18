module Utils where

-------------------------------------------------
-- Some helpful definitions:
-- a ternary, functional version of if-then-else
ifNZ :: Int -> a -> a -> a
ifNZ x tb eb = if (not (x == 0)) then tb else eb

class ZigZag rep where
  demote :: (rep a -> rep b) -> (a -> b)
