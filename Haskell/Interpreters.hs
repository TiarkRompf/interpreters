{-# LANGUAGE TypeFamilies, TypeSynonymInstances, 
    FlexibleInstances, RankNTypes, GADTs,
      DeriveFunctor #-}
module Interpreters where

import Control.Monad
import Control.Applicative
import qualified Data.Function as F

import Language
import Examples
import Utils

-------------------------------------------------
-- Direct interpreter
newtype R a = R {unR :: a} deriving (Show, Functor)

instance Applicative R where
  pure = R
  f <*> x = R $ (unR f) (unR x)

instance Expr R where
  int_ = pure
  plus_ = liftA2 (+)
  times_ = liftA2 (*)

instance ZigZag R where
  demote f = unR . f . R

pull :: (ZigZag rep, Applicative rep) => (rep a -> rep b) -> rep (a -> b)
pull = pure . demote

instance Func R where
  lam = pure . demote
  app = liftA2 ($)
  fix = fmap F.fix . pull

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
-- This is called the "Direct Compiler" in the scala code
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

instance Expr SC where
  int_ = \n -> if n < 0 then SC (\n1 -> ("(" ++ (show n) ++ ")", n1))  else SC $ (\c -> (show n, c))
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

instance Applicative CPS where
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
                                 (e3b,vc3) = unC e3 vc2
                             in (f e1b e2b e3b, vc3))

bindC :: (Int -> AST a -> AST b) -> (C a1 -> C a) -> C b
bindC func f = C(\vc -> let var = C(\vc2 -> (Var vc, vc2))
                            (body, vc') = unC (f var) (succ vc)
                        in (func vc body, vc'))

instance Expr C where
  int_   = toC . INT
  plus_  = twoC Add
  times_ = twoC Mul

instance Func C where
   lam = bindC Lam
   fix = bindC Fix
   app = twoC App

instance IfNZ C where
  ifNonZero = threeC IfNZ

instance Program C where
  type Prog C a b = AST (a -> b)
  prog f = fst (unC (lam f) 0)


-------------------------------------------------
-- CPS Transformer 1
--
-- Rather than hand-merging a compiler and CPS tranformation (like 
-- CPSCompiler in the Scala code), let's be modular about this.
-- In this case, we keep to the language without let.
--
-- Although we can write this 'by hand', without going through
-- the Applicative, this is tedious.  So, here, we'll 
-- restrict to that case.  Unfortunately, neither SC nor AST are
-- Applicative, so there will be tedium in our future
data CPSR repr a = CPSR {unCPSR :: forall res. repr ((a -> res) -> res)}

instance (Func repr, Applicative repr) => Functor (CPSR repr) where
  fmap f (CPSR x) = CPSR (lam $ \k -> app x (lam $ \v -> app k (app (pure f) v)))

instance (Applicative repr, Func repr) => Applicative (CPSR repr) where
  pure i = CPSR $ lam (\k -> app k (pure i))
  (CPSR f) <*> (CPSR a) = 
    CPSR $ lam (\k -> app f (lam (\f' -> app a (lam (\a' -> app k (app f' a'))))))
 
-- even more general version of liftA
liftMA1 :: (Func r) => (r a -> r b) -> CPSR r a -> CPSR r b
liftMA1 f = \(CPSR x) -> CPSR $ lam (\k ->
  app x (lam (\v1 -> app k (f v1))))

liftMA2 :: (Func r) => (r a -> r b -> r c) -> CPSR r a -> CPSR r b -> CPSR r c
liftMA2 f = \(CPSR x) (CPSR y) -> CPSR $ lam (\k ->
  app x (lam (\v1 ->
  app y (lam (\v2 -> app k (f v1 v2))))))

instance (Expr rep, Func rep, Applicative rep) => Expr (CPSR rep) where
  int_ i = pure i -- CPSR $ lam (\k -> app k (int_ i))
  plus_  = liftMA2 plus_
  times_ = liftMA2 times_
