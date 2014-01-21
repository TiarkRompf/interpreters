{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, TypeFamilies #-}
module Language where

-- The core components under our language
class Expr val where
  int_ :: Int -> val Int
  plus_ :: val Int -> val Int -> val Int
  times_ :: val Int -> val Int -> val Int

class IfNZ rep where
  ifNonZero :: rep Int -> rep t -> rep t -> rep t 

-- 'first order' representation of functions
class Func rep where
  lam :: (rep a -> rep b) -> rep (a -> b)
  app :: rep (a -> b) -> (rep a -> rep b)
  fix :: (rep a -> rep a) -> rep a
  
-- 'higher order' representation.  Useful for CPS
class HFunc rep where
  hlam :: (rep a -> rep b) -> rep (rep a -> rep b)
  happ :: rep (rep a -> rep b) -> (rep a -> rep b)
  hfix :: (rep a -> rep a) -> rep a
  
-- but a Func can be made an HFunc:
data I rep a = I {unI :: rep a}

class ZigZag rep where
  demote :: (rep a -> rep b) -> (a -> b)

{-
instance Func rep => HFunc (I rep) where
  -- hlam f = I (lam (unI . f . I))
  happ (I f) (I x) = I $ app f x
-}

-- a program is usually a special case of a lambda term
class Program rep where
  type Prog rep a b 
  prog :: (rep a -> rep b) -> Prog rep a b

type Syntax rep = (Expr rep, Func rep, IfNZ rep)

--
-- Basically, what happens in the Scala version is that the 
-- Labeling trait really changes the signature of everything in a 
-- non-trivial way, even though this is 'invisible' in Scala.
-- Haskell forces one to be explicit about it.

data Label = Root | InLam Label | InThen Label | InElse Label | InFix Label

-- So, rather than functions, we'll call these procedures.
-- and we'll also forget the above modularity
class Proc rep where
  -- a block gives a state transformer. Laziness is important.
  block :: Label -> rep a -> (Label -> (Label, rep a))
  lam' :: (rep a -> rep b) -> rep ((Label, a) -> (Label, b))
