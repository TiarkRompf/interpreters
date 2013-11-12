{-# LANGUAGE MultiParamTypeClasses #-}
module Language where

import Control.Monad (liftM2)

-- The core components under our language, in tagless style
-- val ~ value representation
class SymExpr val b where
  int_ :: Int -> val b
  plus_ :: val b -> val b -> val b
  times_ :: val b -> val b -> val b

-- vrep ~ variable representation
class SymVar vrep a where
  s_ :: String -> vrep a

class SymNZControl c b where
  ifNonZero :: c b -> c () -> c () -> c ()
  whileNonZero :: c b -> c () -> c ()

class Runnable c h where
  run :: c h () -> h

class Mutation m h p v a where
  newRef   :: String -> p a -> m (h p v a) (v a)
  readRef  :: v a -> m (h p v a) (p a)
  writeRef :: v a -> p a -> m (h p v a) ()

-- These combinators are useful to make 'real' programs 
int :: (Monad m, SymExpr p b) => Int -> m (p b)
int = return . int_

plus :: (Monad m, SymExpr p b) => m (p b) -> m (p b) -> m (p b)
plus = liftM2 plus_

newVar nm vl = newRef nm =<< vl
readVar v = readRef v
writeVar nm vl = writeRef nm =<< vl

