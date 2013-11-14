{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds #-}
module Language where

-- The core components under our language, not tagless (i.e. untyped)
class Expr val where
  int_ :: Int -> val Int
  plus_ :: val Int -> val Int -> val Int
  times_ :: val Int -> val Int -> val Int

class IfNZ rep where
  ifNonZero :: rep Int -> rep t -> rep t -> rep t 

class Func rep where
  lam :: (rep a -> rep b) -> rep (a -> b)
  app :: rep (a -> b) -> rep a -> rep b
  fix :: (rep (a -> b) -> rep (a -> b)) -> rep (a -> b)
  
class Program p where
  prog :: (rep a -> rep b) -> p (rep a) (rep b)

type Syntax p rep = (Program p, Expr rep, Func rep, IfNZ rep)
