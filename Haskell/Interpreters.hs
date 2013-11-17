{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module Interpreters where

-- import Control.Applicative
import Control.Monad
import qualified Data.Function as F
import GHC.Exts

import Language
import Examples

-- helpful:
ifNZ x tb eb = if (not (x == 0)) then tb else eb

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

-------------------------------------------------
-- Direct interpreter
newtype R a = R {unR :: a} deriving (Show)

instance Functor R where
  fmap f (R x) = R (f x)

instance MyApp R where
  type Ctx R a = ()
  pure = R
  (R f) <*> (R x) = R $ f x

instance Expr R where
  int_ = pure
  plus_ = liftA2 (+)
  times_ = liftA2 (*)

instance Func R where
  lam f = pure (unR . f . R)
  app f x = f <*> x
  fix = join $ (fmap <*> (return F.fix))

instance IfNZ R where
  ifNonZero = liftA3 (ifNZ)

tester :: (() -> R (a ->  b)) -> (a -> b)
tester f = unR $ f ()

-- and it works (will be translated to a unit test later)
test1 = ((tester fac)(4) == 24 )
-------------------------------------------------
-- String Compiler (needs some context)
newtype SC a = SC {s :: Int -> (String, Int)}

instance Functor SC where
  fmap _ (SC x) = SC x

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
  lam f = SC (\n -> let vs = 'V' : show n
                        var = SC (\n -> (vs, n)) -- can't use pure for typing reasons
                        (body, n1) = s (f var) (n+1)  -- note the +1
                    in ("(\\" ++ vs ++ " -> " ++ body ++ ")", n1))
  app = liftsc2 (\x y -> x ++ " " ++ y)
  fix f = SC (\n -> let vs = 'V' : show n
                        var = SC (\n -> (vs, n))
                        (body, n1) = s (f var) (n+1)
                    in ("fix (\\" ++ vs ++ " -> " ++ body ++ ")", n1))

instance IfNZ SC where
  ifNonZero = liftsc3 (\x y z -> "if (not (" ++ x ++ ") == 0) then " ++ y ++ " else " ++ z)

testersc f = fst $ s (f ()) 0

test2 = (testersc fac ) == 
   "(\\V0 -> fix (\\V1 -> (\\V2 -> if (not (V2) == 0) then V2 * V1 (V2 + (-1)) else 1)) V0)" 
