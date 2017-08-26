{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts  #-}

module LPUtils where

import Control.Monad.LPMonad
import Data.LinearProgram.Common
import Control.Monad.State.Class

class Ord v => LPVar a v where
  var :: a -> v

instance (Monoid v, LPVar a v, LPVar b v) => LPVar (a, b) v where
  var (a, b) = var a `mappend` var b

instance (Monoid v, LPVar a v, LPVar b v, LPVar c v) => LPVar (a, b, c) v where
  var (a, b, c) = var a `mappend` var b `mappend` var c

instance LPVar String String where
  var = id

asLinFunc :: (Ord v, Additive c, Num c) => v -> LinFunc v c
asLinFunc v = linCombination [(1, v)]


defVar :: (Num c, LPVar a v, Group c, MonadState (LP v c) m)
       => a -> LinFunc v c -> m v
defVar a lf = do
  let v = var a
  asLinFunc v `equal` lf
  return v


{-
class Traceable a b where
  match :: a -> b -> Bool
  trace :: [a] -> [b] ->
-}
