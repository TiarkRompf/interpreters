{-# LANGUAGE StandaloneDeriving #-}
module Lattice where

-- Lower Lattice (no top)
class LowerBounded l where
  bottom :: l

class (Eq l, LowerBounded l) => LowerLattice l where
  lub :: l -> l -> l

class UpperBounded l where
  top :: l

-- Upper Lattice
class (Eq l, UpperBounded l) => UpperLattice l where
  glb :: l -> l -> l

-- Complete Lattices
class (LowerLattice l, UpperLattice l) => CLattice l where

leq :: LowerLattice l => l -> l -> Bool
leq a b = a `lub` b == b

-- a flat lattice from a 'flat domain'
-- the domain restriction will be enforced on the instances
data FlatLattice d = Bottom | Top | Embed d
deriving instance Eq d => Eq (FlatLattice d)

-- for sharing purposes, we take the 'core' of a lattice as input 
-- for a general lattice; no domain constraint
data GLattice d = GBottom | GTop | GEmbed d
deriving instance Eq d => Eq (GLattice d)

flatlub :: Eq d => FlatLattice d -> FlatLattice d -> FlatLattice d
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

instance Eq d => LowerBounded (FlatLattice d) where
  bottom = Bottom

instance Eq d => LowerLattice (FlatLattice d) where
  lub = flatlub

instance Eq d => UpperBounded (FlatLattice d) where
  top = Top

instance Eq d => LowerBounded (GLattice d) where
  bottom = GBottom

instance Eq d => LowerLattice (GLattice d) where
  lub = glub

instance Eq d => UpperBounded (GLattice d) where
  top = GTop

