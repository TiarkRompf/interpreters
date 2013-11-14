module Interpreters where

import Control.Applicative
import Control.Monad
import qualified Data.Function as F

import Language
import Examples

-- helpful:
ifNZ x tb eb = if (not (x == 0)) then tb else eb

-------------------------------------------------
-- Direct interpreter
newtype R a = R {unR :: a} deriving (Show)

instance Functor R where
  fmap f (R x) = R (f x)

instance Applicative R where
  pure = R
  (R f) <*> (R x) = R $ f x

instance Expr R where
  int_ = pure
  plus_ = liftA2 (+)
  times_ = liftA2 (*)

instance Func R where
  lam f = pure (unR . f . R)
  app f x = f <*> x
  fix = join $ fmap <*> return F.fix

instance IfNZ R where
  ifNonZero = liftA3 (ifNZ)

type P a b = a -> b

instance Program (->) where
  prog f = f

-- and our 'fac' program is interpretable:
rfac :: P (R Int) (R Int)
rfac = fac 

tester :: P (R a) (R b) -> (a -> b)
tester f = unR . f . R

-- and it works (will be translated to a unit test later)
test1 = ((tester rfac)(4) == 24 )
-------------------------------------------------
-- String Compiler (needs some context)
newtype SC a = SC {s :: Int -> String}

instance Expr SC where
  int_ n = SC $ \_ -> show n
  plus_ x y = SC $ \n -> s x n ++ " + " ++ (s y n)
  times_ x y = SC $ \n -> s x n ++ " * " ++ (s y n)
