{-# LANGUAGE TypeFamilies, TypeSynonymInstances, 
    FlexibleInstances, RankNTypes, GADTs, ConstraintKinds,
      DeriveFunctor #-}
module Interpreters where

import Control.Monad
import qualified Data.Function as F

import Language
import Examples
import Utils

-------------------------------------------------
-- Direct interpreter
newtype R a = R {unR :: a} deriving (Show, Functor)

instance MyApp R where
  type Ctx R a = ()
  pure = R
  f <*> x = R $ (unR f) (unR x)

instance Expr R where
  int_ = pure
  plus_ = liftA2 (+)
  times_ = liftA2 (*)

instance ZigZag R where
  demote f = unR . f . R

pull :: (ZigZag rep, MyApp rep, Ctx rep (a->b)) => 
    (rep a -> rep b) -> rep (a -> b)
pull = pure . demote

instance Func R where
  lam = pure . demote
  app = liftA2 ($)
  fix = liftA1 F.fix . pull

instance IfNZ R where
  ifNonZero = liftA3 (ifNZ)

instance Program R where
  type Prog R a b = a -> b
  prog = demote

-- uses Haskell's laziness
tester :: R (a -> b) -> (a -> b)
tester = unR

-- and it works (will be translated to a unit test later)
test1 = ((tester fac)(4) == 24 )

-------------------------------------------------
-- String Compiler (needs some context)
newtype SC a = SC {s :: Int -> (String, Int)}

instance Functor SC where
  fmap _ (SC x) = SC x

-- These are like liftA, liftA1 and liftA2, but not as polymorphic.
-- Should make a class out of this setup, later.
liftsc1 :: (String -> String) -> SC a -> SC a
liftsc1 f x = SC (\n -> let (e1,n1) = s x n
                        in (f e1, n1) )

liftsc2 :: (String -> String -> String) -> SC a -> SC b -> SC c
liftsc2 f x y = SC (\n -> let (e1,n1) = s x n
                              (e2,n2) = s y n1
                          in (f e1 e2, n2) )

liftsc3 :: (String -> String -> String -> String) -> SC a -> SC b -> SC c -> SC d
liftsc3 f x y z = SC (\n -> let (e1,n1) = s x n
                                (e2,n2) = s y n1
                                (e3,n3) = s z n2
                            in (f e1 e2 e3, n3) )

instance MyApp SC where
  type Ctx SC a = Show a
  pure x = SC $ \n -> (show x, n)
  f <*> x = SC (\n -> let (f1, n1) = s f n
                          (x1, n2) = s x n1
                      in (f1 ++ " " ++ x1, n2))

instance Expr SC where
  int_ = \n -> if n < 0 then SC (\n1 -> ("(" ++ (show n) ++ ")", n1))  else pure n
  plus_ = liftsc2 (\x y -> "(" ++ x ++ " + " ++ y ++ ")")
  times_ = liftsc2 (\x y -> x ++ " * " ++ y)

instance Func SC where
  lam f = SC (\n -> let vs = 'y' : show n
                        var = SC (\n -> (vs, n)) -- can't use pure for typing reasons
                        (body, n1) = s (f var) (n+1)  -- note the +1
                    in ("(\\" ++ vs ++ " -> " ++ body ++ ")", n1))
  app = liftsc2 (\x y -> x ++ " " ++ y)
  fix f = SC (\n -> let vs = 'y' : show n
                        var = SC (\n -> (vs, n))
                        (body, n1) = s (f var) (n+1)
                    in ("fix (\\" ++ vs ++ " -> " ++ body ++ ")", n1))

instance IfNZ SC where
  ifNonZero = liftsc3 (\x y z -> "if (not (" ++ x ++ ") == 0) then " ++ y ++ " else " ++ z)

instance Program SC where
  type Prog SC a b = String
  prog f = fst $ s (f (SC $ \n -> ("y",n))) 0

testersc f = fst $ s f 0

test2 = testersc fac == 
   "(\\y0 -> fix (\\y1 -> (\\y2 -> if (not (y2) == 0) then y2 * y1 (y2 + (-1)) else 1)) y0)" 

-------------------------------------------------
-- CBN CPS Interpreter
newtype CPS t = CPS (forall res. (t -> res) -> res)
unCPS (CPS x) = x

instance Functor CPS where
  fmap f (CPS t) = CPS $ \k -> t (k . f)

instance MyApp CPS where
  type Ctx CPS a = ()
  pure a = CPS ($ a)
  (CPS f) <*> (CPS a) = CPS $ \k -> f (\f' -> a (\a' -> k $ f' a'))

instance Expr CPS where
  int_ = pure
  plus_ = liftA2 (+)
  times_ = liftA2 (*)

instance HFunc CPS where
  hlam f = pure f
  -- the main difference in CBN is below:
  happ f x = CPS $ \k -> (unCPS f) (\f' -> unCPS (f' x) k)
  hfix = join $ (fmap <*> (return F.fix))

instance IfNZ CPS where
  ifNonZero = liftA3 (ifNZ)

instance Program CPS where
  type Prog CPS a b = (a -> b)
  prog f = \a -> (unCPS $ f (pure a)) id

-------------------------------------------------
-- AST Compiler
--
-- This uses a GADT; this is a nicer version of the Haskell code
-- from the JFP paper

data AST t where
    Var :: Int -> AST t                -- variables identified by numbers
    INT :: Int -> AST Int
    Add :: AST Int -> AST Int -> AST Int
    Mul :: AST Int -> AST Int -> AST Int
    IfNZ :: AST Int -> AST t -> AST t -> AST t
    Lam :: Int -> AST t2 -> AST (t1->t2)
    App :: AST (t1->t2) -> AST t1  -> AST t2
    Fix :: Int -> AST t -> AST t
    LIFT :: t -> AST t                 -- lift values.  Needed for lam.

newtype C t = C (Int -> (AST t, Int))

-- Helper functors for the AST evaluator
unC (C x) = x

toC :: AST a -> C a
toC x = C(\vc -> (x, vc))

oneC :: (AST a -> AST b) -> C a -> C b
oneC f e1 = C(\vc -> let (e1b,vc1) = unC e1 vc
                     in (f e1b ,vc1))
twoC f e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                            (e2b,vc2) = unC e2 vc1
                         in (f e1b e2b,vc2))
threeC :: (AST a -> AST b -> AST c -> AST d) -> C a -> C b -> C c -> C d
threeC f e1 e2 e3 = C(\vc -> let (e1b,vc1) = unC e1 vc
                                 (e2b,vc2) = unC e2 vc1
                                 (e3b,vc3) = unC e3 vc1
                             in (f e1b e2b e3b, vc3))

instance Expr C where
  int_   = toC . INT
  plus_  = twoC Add
  times_ = twoC Mul

instance Func C where
   lam f = C(\vc -> let var = C(\vc2 -> (Var vc, vc2))
                        (body, vc') = unC (f var) (succ vc)
                   in (Lam vc body, vc'))
   fix f = C(\vc -> let var = C(\vc2 -> (Var vc, vc2))
                        (body, vc') = unC (f var) (succ vc)
                   in (Fix vc body, vc'))
   app = twoC App

instance IfNZ C where
  ifNonZero = threeC IfNZ

instance Program C where
  type Prog C a b = AST (a -> b)
  prog f = fst (unC (lam f) 0)

