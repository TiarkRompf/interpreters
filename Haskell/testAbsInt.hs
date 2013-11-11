{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, 
             FlexibleInstances, KindSignatures, StandaloneDeriving,
             FlexibleContexts, UndecidableInstances
             #-}
module Main where

-- Note about Show instances: they are not meant to be Read, they are for
-- human consumption.  They can be reverted to being 'derived' for that.

import Prelude hiding (lookup)
import qualified Data.Map as Map
-- import Control.Monad (liftM2, unless)
import Control.Applicative ((<$>))
import Control.Monad.Identity
import qualified Control.Monad.Trans.State as S
import Data.Maybe (fromJust)
import Data.Bits ((.&.))
import Data.Char (ord)

import Lattice
import Domains

-- For 'Abstract' interpretation 
-- b is an abstraction of values of a
class Abstract a b where
    abstract :: a -> b

instance Abstract Int Sign where
  abstract 0 = Zero
  abstract x | x < 0     = Neg
             | otherwise = Pos

instance Abstract Int Parity where
  abstract x = if x .&. 1 == 0 then Even else Odd

-- no abstraction at all
instance Abstract a a where
  abstract x = x

instance Abstract a b => Abstract (FlatLattice a) (FlatLattice b) where
  abstract Bottom = Bottom
  abstract Top    = Top
  abstract (Embed x) = Embed (abstract x)

instance Abstract a b => Abstract (GLattice a) (GLattice b) where
  abstract GBottom    = GBottom
  abstract GTop       = GTop
  abstract (GEmbed x) = GEmbed (abstract x)

instance Abstract Int a => Abstract Int (FlatLattice a) where
   abstract x = Embed (abstract x)

instance Abstract Int a => Abstract Int (GLattice a) where
   abstract x = GEmbed (abstract x)

-- actually the same as for Sign
instance Abstract Int NSign where
  abstract 0 = NSZero
  abstract x | x < 0     = NSNeg
             | otherwise = NSPos
   
class Additive a where
    add :: a -> a -> a

instance Additive Sign where
    add = splus 

instance Additive Parity where
    add = pplus

instance Additive Int where
    add = (+)

instance Additive NSign where
    add = nsplus

instance Additive a => Additive (FlatLattice a) where
    add = liftFL2 add

instance Additive a => Additive (GLattice a) where
    add = liftGL2 add

maybeNonZero :: (Eq b, Abstract Int b, LowerLattice b) => b -> Bool
maybeNonZero = \a -> not (a `leq` (abstract (0::Int)))

maybeZero :: (Eq b, Abstract Int b, LowerLattice b) => b -> Bool
maybeZero = \a -> zero `leq` a
  where zero = abstract (0::Int)
           
-- The core components under our language, in tagless style
class SymExpr prep b where
  int_ :: Int -> prep b
  plus_ :: prep b -> prep b -> prep b

class SymVar vrep a where
  s_ :: String -> vrep a

class Heap h p v a where
  insert :: v a -> p a -> h p v a -> h p v a
  lookup :: v a -> h p v a -> Maybe (p a)
  empty :: h p v a

instance (Abstract Int b, Additive b) => SymExpr FlatLattice b where
  int_ x = Embed $ abstract x
  plus_ = add
 
instance (Abstract Int b, Additive b) => SymExpr GLattice b where
  int_ x = GEmbed $ abstract x
  plus_ = add
 
class Mutation m h p v a where
  newRef   :: String -> p a -> m (h p v a) (v a)
  readRef  :: v a -> m (h p v a) (p a)
  writeRef :: v a -> p a -> m (h p v a) ()

newtype ST s a = ST (S.State s a)
unST (ST x) = x

instance Monad (ST s) where
  return = ST . return
  (ST m) >>= k = ST $ m >>= unST . k

instance (Heap h p v a, SymVar v a) => Mutation ST h p v a where
  newRef nm l = ST $ do S.modify $ insert (s_ nm) l
                        return $ s_ nm
  readRef vr = ST $ fromJust . lookup vr <$> S.get
  writeRef vr l = ST $ S.modify $ insert vr l

class SymControl c h (p :: * -> *) (v :: * -> *) a b where
  ifNonZero :: c (h p v a) b -> c (h p v a) () -> c (h p v a) () -> c (h p v a) ()
  whileNonZero :: c (h p v a) b -> c (h p v a) () -> c (h p v a) ()

class Runnable c h (p :: * -> *) (v :: * -> *) a where
  run :: c (h p v a) () -> h p v a

instance (LowerLattice b, Eq b, Heap h p v a, LowerLattice (h p v a),
  Eq (h p v a), Abstract Int b) 
    => SymControl ST h p v a b where
  ifNonZero cond statTrue statFalse = ST $
    do c <- unST cond
       s <- S.get
       let sta = if (maybeNonZero c) then S.execState (unST statTrue) s else empty
       let stb = if (maybeZero c) then S.execState (unST statFalse) s else empty
       S.put $ lub sta stb

  whileNonZero cond body = ST $
    do st0 <- S.get
       unST $ ifNonZero cond body  $ ST (S.state (\s -> ((),s)))
       st1 <- S.get
       unless (st1 == st0) (unST $ whileNonZero cond body)

instance (Heap h p v a) => Runnable ST h p v a where
  run (ST body) = S.execState body empty
    
-- The explicit ST below forces a number of the instances above to be chosen
program :: (Heap h p v a, Abstract Int a, LowerLattice (h p v a),
  Eq (h p v a))
  => ST (h p v a) () -> h p v a
program body = run body
       
-- An actual instance to test with
newtype Var a = V String deriving (Ord, Eq)
instance Show (Var a) where
  show (V x) = x

instance SymVar Var a where
  s_ = V

-- Another
newtype IVar a = IV Int deriving (Ord, Eq)
instance Show (IVar a) where
  show (IV x) = show x

instance SymVar IVar a where
  s_ s = IV (foldr (\c n -> n*2^8 + ord c) 0 s)

data MapHeap p v a = H { unH :: Map.Map (v a) (p a) }

deriving instance (Eq a, Eq (v a), Eq (p a)) => Eq (MapHeap p v a)
instance (Show (p a), Show (v a)) => Show (MapHeap p v a) where
  show (H x) = show x

instance Ord (v a) => Heap MapHeap p v a where
  insert v r h = H $ Map.insert v r (unH h)
  lookup v h = Map.lookup v (unH h)
  empty = H $ Map.empty

instance Ord (v a) => LowerBounded (MapHeap p v a) where
  bottom = empty

instance (Eq a, Ord (v a), LowerLattice (p a)) => LowerLattice (MapHeap p v a) where
  lub s1 s2 = H $ Map.foldrWithKey (Map.insertWith lub) (unH s1) (unH s2)

-- These combinators are even more useful to make 'real' programs 
int :: (Heap h p v a, SymExpr p b, Abstract Int b) => 
       Int -> ST (h p v a) (p b)
int = return . int_

plus = liftM2 plus_
newVar nm vl = newRef nm =<< vl
readVar v = readRef v
writeVar nm vl = writeRef nm =<< vl

-- type signatures needed because programs are so polymorphic!
prog1 :: (Eq b, Abstract Int b, Additive b,
  SymVar v b, Ord (v b), LowerLattice (p b),
  Heap h p v b, LowerLattice (h p v b), Eq (h p v b),
  Abstract Int (p b), Eq (p b), SymExpr p b) => h p v b
prog1 = program (
  do 
    c <- newVar "c" (int 0)
    whileNonZero (readVar c) (
      writeVar c (plus (readVar c) (int 1)) ) )
  
prog2 :: (Eq b, Abstract Int b, Additive b,
  SymVar v b, Ord (v b), LowerLattice (p b),
  Heap h p v b, LowerLattice (h p v b), Eq (h p v b),
  Abstract Int (p b), Eq (p b), SymExpr p b) => h p v b
prog2 = program (
  do 
    c <- newVar "c" (int 0)
    whileNonZero const1 (
      do writeVar c (plus (readVar c) (int 1)) ) )
  where
    -- the signature is needed because Haskell correctly infers that 
    -- this constant can live somewhere else than the global ones
    const1 :: (Abstract Int b0, SymExpr p0 b0, Heap h0 p0 v0 b0) => ST (h0 p0 v0 b0) (p0 b0)
    const1 = int 1
  
prog3 :: (Abstract Int b,
  SymVar v b, Ord (v b),
  Heap h p v b, LowerLattice (h p v b), Eq (h p v b),
  Abstract Int (p b), LowerLattice (p b), Eq (p b), SymExpr p b) => 
  Int -> h p v b
prog3 = \n -> program (
  do 
    c <- newVar "c" (int n)
    ifNonZero (readVar c)
        (writeVar c (int 7))
        (writeVar c (int (-4))) )

prog4 = program (
  do 
    c <- newRef "c" (top)
    x <- newVar "x" (int 0)
    whileNonZero (readVar c)
        (writeVar x (int 7)) )

-- Be monomorphic to be able to print something
type AS a = MapHeap FlatLattice Var a

main = do putStrLn $ show (prog1 :: AS Sign)
          putStrLn $ show (prog2 :: AS Sign)
          putStrLn $ show (prog1 :: AS Parity)
          putStrLn $ show (prog1 :: AS Int)
          putStrLn $ show (prog1 :: MapHeap GLattice Var NSign)
          putStrLn $ show (prog1 :: MapHeap GLattice IVar Sign)
          putStrLn $ show (prog3 5 :: AS Parity)
          putStrLn $ show (prog3 5 :: AS Int)
          putStrLn $ show (prog3 0 :: AS Parity)
          putStrLn $ show (prog3 0 :: AS Int)
          putStrLn $ show (prog3 0 :: AS Sign)
          putStrLn $ show (prog4 :: AS Sign)
