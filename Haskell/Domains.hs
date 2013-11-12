-- Some domains useful to Abstract Interpretation
module Domains where

-- import Lattice

-- a class with no methods, just to allow us to declare domains
class Domain d where
instance Domain Int
instance Domain Bool

--    Sign
data Sign = Neg | Zero | Pos deriving (Eq, Show)
instance Domain Sign

-- this is partial, but won't be used raw
splus :: Sign -> Sign -> Sign
splus Pos Pos = Pos
splus Neg Neg = Neg
splus Zero x  = x
splus x Zero  = x

stimes :: Sign -> Sign -> Sign
stimes Pos Pos = Pos
stimes Neg Neg = Pos
stimes Zero _  = Zero
stimes _ Zero  = Zero
stimes Pos Neg = Neg
stimes Neg Pos = Neg

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

nstimes :: NSign -> NSign -> NSign
nstimes NSPos  NSPos  = NSPos
nstimes NSNeg  NSNeg  = NSPos
nstimes NSNeg  NSPos  = NSNeg
nstimes NSPos  NSNeg  = NSNeg
nstimes NSPos  NSZNeg = NSZNeg
nstimes NSZero _      = NSZero
nstimes _      NSZero = NSZero
nstimes NSNeg  NSZNeg = NSZPos
nstimes NSNeg  NSZPos = NSZNeg
nstimes NSPos  NSZPos = NSZPos
nstimes NSZNeg NSZNeg = NSZPos
nstimes NSZNeg NSNeg  = NSZPos
nstimes NSZNeg NSPos  = NSZNeg
nstimes NSZNeg NSZPos = NSZNeg
nstimes NSZPos NSPos  = NSZPos
nstimes NSZPos NSNeg  = NSZNeg
nstimes NSZPos NSZNeg = NSZNeg
nstimes NSZPos NSZPos = NSZPos

-- Parity
data Parity = Even | Odd deriving (Eq, Show)
instance Domain Parity

pplus :: Parity -> Parity -> Parity
pplus Even Even = Even
pplus Odd  Odd  = Even
pplus Odd  Even = Odd
pplus Even Odd  = Odd

ptimes :: Parity -> Parity -> Parity
ptimes Even Even = Even
ptimes Odd  Odd  = Odd
ptimes Odd  Even = Even
ptimes Even Odd  = Even
