{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Abstract where

import Data.Bits ((.&.))

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

class Multiplicative a where
    mul :: a -> a -> a

instance Multiplicative Sign where
    mul = stimes

instance Multiplicative Parity where
    mul = ptimes

instance Multiplicative Int where
    mul = (*)

instance Multiplicative NSign where
    mul = nstimes

instance Multiplicative a => Multiplicative (FlatLattice a) where
    mul = liftFL2 mul

instance Multiplicative a => Multiplicative (GLattice a) where
    mul = liftGL2 mul

maybeNonZero :: (Eq b, Abstract Int b, LowerLattice b) => b -> Bool
maybeNonZero = \a -> not (a `leq` (abstract (0::Int)))

maybeZero :: (Eq b, Abstract Int b, LowerLattice b) => b -> Bool
maybeZero = (zero `leq`)
  where zero = abstract (0::Int)
