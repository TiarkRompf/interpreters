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
