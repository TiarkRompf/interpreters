{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, 
             StandaloneDeriving, FlexibleInstances,
             FlexibleContexts
             #-}
module Main where

-- Note about Show instances: they are not meant to be Read, they are for
-- human consumption.  They can be reverted to being 'derived' for that.

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Control.Monad (liftM2, unless)
import Control.Applicative ((<$>))
import Control.Monad.Identity
import qualified Control.Monad.Trans.State as S
import Data.Maybe (fromJust)
import Data.Bits ((.&.))
import Data.Char (ord)

-- Lower Lattice (no top)
class LowerLattice l where
  bottom :: l
  lub :: l -> l -> l

-- Complete Lattices
class LowerLattice l => CLattice l where
  top :: l

leq :: (Eq l, LowerLattice l) => l -> l -> Bool
leq a b = a `lub` b == b

-- a flat lattice from a 'flat domain'
-- the domain restriction will be enforced on the instances
data FlatLattice d = Bottom | Top | Embed d
deriving instance Eq d => Eq (FlatLattice d)

-- for sharing purposes, we take the 'core' of a lattice as input 
-- for a general lattice; no domain constraint
data GLattice d = GBottom | GTop | GEmbed d
deriving instance Eq d => Eq (GLattice d)

flatlub :: (Domain d, Eq d) => FlatLattice d -> FlatLattice d -> FlatLattice d
flatlub Bottom b = b
flatlub a Bottom = a
flatlub (Embed a) (Embed b) | a == b = Embed a
flatlub _ _ = Top

liftFL2 :: (d -> d -> d) -> FlatLattice d -> FlatLattice d -> FlatLattice d
liftFL2 f (Embed a) (Embed b) = Embed (f a b)
liftFL2 _ Bottom a = a
liftFL2 _ a Bottom = a
liftFL2 _ _ _ = Top

instance Show d => Show (FlatLattice d) where
  show Bottom    = "Bottom"
  show Top       = "Top"
  show (Embed d) = show d

instance Show d => Show (GLattice d) where
  show GBottom    = "Bottom"
  show GTop       = "Top"
  show (GEmbed d) = show d

glub :: Eq d => GLattice d -> GLattice d -> GLattice d
glub GBottom b = b
glub a GBottom = a
glub (GEmbed a) (GEmbed b) | a == b = GEmbed a
glub _ _ = GTop

liftGL2 :: (d -> d -> d) -> GLattice d -> GLattice d -> GLattice d
liftGL2 f (GEmbed a) (GEmbed b) = GEmbed (f a b)
liftGL2 _ GBottom a = a
liftGL2 _ a GBottom = a
liftGL2 _ _ _ = GTop

-- an class with no methods, just to allow us to declare domains
class Domain d where
instance Domain Int
instance Domain Bool

instance (Domain d, Eq d) => LowerLattice (FlatLattice d) where
  bottom = Bottom
  lub = flatlub

instance (Domain d, Eq d) => CLattice (FlatLattice d) where
  top = Top

instance Eq d => LowerLattice (GLattice d) where
  bottom = GBottom
  lub = glub

instance Eq d => CLattice (GLattice d) where
  top = GTop

--    Sign
data Sign = Neg | Zero | Pos deriving (Eq, Show)
instance Domain Sign

-- this is partial, but won't be used raw
splus :: Sign -> Sign -> Sign
splus Pos Pos = Pos
splus Neg Neg = Neg
splus Zero x  = x
splus x Zero  = x

-- NSign, a more refined lattice
data NSign = NSPos | NSNeg | NSZero | NSZNeg | NSZPos 
    deriving (Eq, Show)
-- not a Domain

-- partial
nsplus :: NSign -> NSign -> NSign
nsplus NSPos  NSPos  = NSPos
nsplus NSNeg  NSNeg  = NSNeg
nsplus NSZero x      = x
nsplus x      NSZero = x
nsplus NSNeg  NSZNeg = NSNeg
nsplus NSPos  NSZPos = NSPos
nsplus NSZNeg NSZNeg = NSZNeg
nsplus NSZNeg NSNeg  = NSNeg
nsplus NSZPos NSPos  = NSPos
nsplus NSZPos NSZPos = NSZPos

-- Parity
data Parity = Even | Odd deriving (Eq, Show)
instance Domain Parity

pplus :: Parity -> Parity -> Parity
pplus Even Even = Even
pplus Odd  Odd  = Even
pplus Odd  Even = Odd
pplus Even Odd  = Odd

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
   
class Abstract Int a => Additive a where
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
  where zero :: Abstract Int b => b
        zero = abstract (0::Int)
           
-- The core components under our language, in tagless style
class SymExpr prep where
  int_ :: Abstract Int b => Int -> prep b
  plus_ :: Additive b => prep b -> prep b -> prep b

class SymVar vrep where
  s_ :: String -> vrep a

class Heap h where
  insert :: Ord (v a) => v a -> p a -> h p v a -> h p v a
  lookup :: Ord (v a) => v a -> h p v a -> Maybe (p a)
  empty :: h p v a

instance SymExpr FlatLattice where
  int_ x = Embed $ abstract x
  plus_ = add
 
instance SymExpr GLattice where
  int_ x = GEmbed $ abstract x
  plus_ = add
 
class Heap h => Mutation m h p v a where
  newRef   :: (SymVar v, Ord (v a)) => String -> p a -> m (h p v a) (v a)
  readRef  :: (Ord (v a)) => v a -> m (h p v a) (p a)
  writeRef :: (Ord (v a)) => v a -> p a -> m (h p v a) ()

newtype ST s a = ST (S.State s a)
unST (ST x) = x

instance Monad (ST s) where
  return = ST . return
  (ST m) >>= k = ST $ m >>= unST . k

instance Heap h => Mutation ST h p v a where
  newRef nm l = ST $ do S.modify $ insert (s_ nm) l
                        return $ s_ nm
  readRef vr = ST $ fromJust . lookup vr <$> S.get
  writeRef vr l = ST $ S.modify $ insert vr l

class SymControl c where
  ifNonZero :: (Heap h, Abstract Int b,  Eq b, LowerLattice b,
    Ord (v a), LowerLattice (h p v a)) =>
      c (h p v a) b -> c (h p v a) () -> c (h p v a) () -> c (h p v a) ()
  whileNonZero :: (Heap h, Ord (v a), CLattice (p a), Eq (h p v a), 
    Abstract Int b, CLattice b, Eq b, LowerLattice (h p v a)) =>
      c (h p v a) b -> c (h p v a) () -> c (h p v a) ()
  run :: Heap h => c (h p v a) b -> h p v a

instance SymControl ST where
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

  run (ST body) = S.execState body empty
    
-- The explicit ST below forces a number of the instances above to be chosen
program :: (Heap h) => ST (h p v a) b -> h p v a
program body = run body
       
-- An actual instance to test with
newtype Var a = V String deriving (Ord, Eq)
instance Show (Var a) where
  show (V x) = x

instance SymVar Var where
  s_ = V

-- Another
newtype IVar a = IV Int deriving (Ord, Eq)
instance Show (IVar a) where
  show (IV x) = show x

instance SymVar IVar where
  s_ s = IV (foldr (\c n -> n*2^8 + ord c) 0 s)

data MapHeap p v a = H { unH :: Map.Map (v a) (p a) }

deriving instance (Eq a, Eq (v a), Eq (p a)) => Eq (MapHeap p v a)
instance (Show (p a), Show (v a)) => Show (MapHeap p v a) where
  show (H x) = show x

instance Heap MapHeap where
  insert v r h = H $ Map.insert v r (unH h)
  lookup v h = Map.lookup v (unH h)
  empty = H $ Map.empty

instance (Eq a, Ord (v a), LowerLattice (p a)) => LowerLattice (MapHeap p v a) where
  bottom = empty
  lub s1 s2 = H $ Map.foldrWithKey (Map.insertWith lub) (unH s1) (unH s2)

-- These combinators are even more useful to make 'real' programs 
int :: (Heap h, SymExpr p, Abstract Int b) => 
       Int -> ST (h p v a) (p b)
int = return . int_

plus = liftM2 plus_
newVar nm vl = newRef nm =<< vl
readVar v = readRef v
writeVar nm vl = writeRef nm =<< vl

-- type signatures needed because programs are so polymorphic!
prog1 :: (Eq b, Abstract Int b, Additive b,
  SymVar v, Ord (v b),
  Heap h, LowerLattice (h p v b), Eq (h p v b),
  Abstract Int (p b), CLattice (p b), Eq (p b), SymExpr p) => h p v b
prog1 = program (
  do 
    c <- newVar "c" (int 0)
    whileNonZero (readVar c) (
      writeVar c (plus (readVar c) (int 1)) ) )
  
prog2 :: (Eq b, Abstract Int b, Additive b,
  SymVar v, Ord (v b),
  Heap h, LowerLattice (h p v b), Eq (h p v b),
  Abstract Int (p b), CLattice (p b), Eq (p b), SymExpr p) => h p v b
prog2 = program (
  do 
    c <- newVar "c" (int 0)
    whileNonZero const1 (
      do writeVar c (plus (readVar c) (int 1)) ) )
  where
    -- the signature is needed because Haskell correctly infers that 
    -- this constant can live somewhere else than the global ones
    const1 :: (Abstract Int b0, SymExpr p0, Heap h0) => ST (h0 p0 v0 b0) (p0 b0)
    const1 = int 1
  
prog3 :: (Abstract Int b,
  SymVar v, Ord (v b),
  Heap h, LowerLattice (h p v b),
  Abstract Int (p b), LowerLattice (p b), Eq (p b), SymExpr p) => 
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
