{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, 
             FlexibleInstances, KindSignatures, StandaloneDeriving,
             FlexibleContexts, UndecidableInstances, TypeFamilies,
             ConstraintKinds
             #-}
module Main where

-- Note about Show instances: they are not meant to be Read, they are for
-- human consumption.  They can be reverted to being 'derived' for that.

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Control.Applicative ((<$>))
import Control.Monad.Identity
import qualified Control.Monad.Trans.State as S
import Data.Maybe (fromJust)
import Data.Char (ord)
import GHC.Exts

import Lattice
import Domains
import Abstract
import Language

-- instances of the language that can be interpreted
instance (Abstract Int b, Additive b, Multiplicative b) => SymExpr FlatLattice b where
  int_ x = Embed $ abstract x
  plus_ = add
  times_ = mul
 
instance (Abstract Int b, Additive b, Multiplicative b) => SymExpr GLattice b where
  int_ x = GEmbed $ abstract x
  plus_ = add
  times_ = mul
 
class Empty s where
  empty :: s

class Heap h where
  type HeapCtx h :: * -> Constraint
  insert :: HeapCtx h var => var -> val -> h val var -> h val var
  lookup :: HeapCtx h var => var -> h val var -> Maybe val

newtype ST s a = ST (S.State s a)
unST (ST x) = x

instance Monad (ST s) where
  return = ST . return
  (ST m) >>= k = ST $ m >>= unST . k

instance (Heap h, HeapCtx h (v a), SymVar v a) => Mutation ST h (p a) (v a) where
  newRef nm l = ST $ do S.modify $ insert (s_ nm) l
                        return $ s_ nm
  readRef vr = ST $ fromJust . lookup vr <$> S.get
  writeRef vr l = ST $ S.modify $ insert vr l

instance (LowerLattice b, Eq b, Abstract Int b, Eq s, Empty s, LowerLattice s) => 
    SymNZControl (ST s) b where
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

instance (Heap h, Empty (h p v)) => Runnable ST (h p v) where
  run (ST body) = S.execState body empty
    
-- The explicit ST below forces a number of the instances above to be chosen
program :: (Heap h, Empty (h p v)) => ST (h p v) () -> h p v
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

data MapHeap p v = H { unH :: Map.Map v p }

deriving instance (Eq a, Eq (v a), Eq (p a)) => Eq (MapHeap (p a) (v a))
instance (Show (p a), Show (v a)) => Show (MapHeap (p a) (v a)) where
  show (H x) = show x

instance Empty (MapHeap (p a) (v a)) where
  empty = H $ Map.empty

instance Heap MapHeap where
  type HeapCtx MapHeap = Ord
  insert v r h = H $ Map.insert v r (unH h)
  lookup v h = Map.lookup v (unH h)

instance LowerBounded (MapHeap (p a) (v a)) where
  bottom = empty

instance (Eq a, Ord (v a), LowerLattice (p a)) => LowerLattice (MapHeap (p a) (v a)) where
  lub s1 s2 = H $ Map.foldrWithKey (Map.insertWith lub) (unH s1) (unH s2)

-- type signatures needed
pprog1 :: (SymExpr p a, Monad (m (h (p a) (v a))), Mutation m h (p a) (v a), 
   SymNZControl (m (h (p a) (v a))) (p a)) => m (h (p a) (v a)) ()
pprog1 = 
  do 
    c <- newVar "c" (int 0)
    whileNonZero (readVar c) (
      writeVar c (plus (readVar c) (int 1)) )
   
prog1 = program pprog1

prog2 :: (Eq b, Abstract Int b, Additive b,
  SymVar v b, Ord (v b), LowerLattice (p b),
  Heap h, LowerLattice (h (p b) (v b)), Eq (h (p b) (v b)),
  Empty (h (p b) (v b)), HeapCtx h (v b),
  Abstract Int (p b), Eq (p b), SymExpr p b) => h (p b) (v b)
prog2 = program (
  do 
    c <- newVar "c" (int 0)
    whileNonZero const1 (
      do writeVar c (plus (readVar c) (int 1)) ) )
  where
    -- the signature is needed because Haskell correctly infers that 
    -- this constant can live somewhere else than the global ones
    const1 :: (Abstract Int b0, SymExpr p0 b0, Heap h0) => ST (h0 (p0 b0) (v0 b0)) (p0 b0)
    const1 = int 1
  
prog3 :: (Abstract Int b,
  SymVar v b, Ord (v b),
  Heap h, LowerLattice (h (p b) (v b)), Eq (h (p b) (v b)),
  Empty (h (p b) (v b)), HeapCtx h (v b),
  Abstract Int (p b), LowerLattice (p b), Eq (p b), SymExpr p b) => 
  Int -> h (p b) (v b)
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
type AS a = MapHeap (FlatLattice a) (Var a)

main = do putStrLn $ show (prog1 :: AS Sign)
          putStrLn $ show (prog2 :: AS Sign)
          putStrLn $ show (prog1 :: AS Parity)
          putStrLn $ show (prog1 :: AS Int)
          putStrLn $ show (prog1 :: MapHeap (GLattice NSign) (Var NSign))
          putStrLn $ show (prog1 :: MapHeap (GLattice Sign) (IVar Sign))
          putStrLn $ show (prog3 5 :: AS Parity)
          putStrLn $ show (prog3 5 :: AS Int)
          putStrLn $ show (prog3 0 :: AS Parity)
          putStrLn $ show (prog3 0 :: AS Int)
          putStrLn $ show (prog3 0 :: AS Sign)
          putStrLn $ show (prog4 :: AS Sign)
