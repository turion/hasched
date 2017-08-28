{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts  #-}

module LPUtils where

import Control.Monad.LPMonad
import Data.LinearProgram.Common
import Control.Monad.State.Class

class Ord v => LPVar a v where
  -- Creates the variable name
  var :: a -> v
  -- Recovers the value from a list of possible values
  val :: [a] -> v -> Maybe a
  val as v = lookup v $ zip (var <$> as) as

class LPVal b where
  fromDouble :: Double -> Maybe b

instance LPVal Bool where
  -- TODO Disclaimer: I have no idea how rounding will play here
  fromDouble 0 = Just False
  fromDouble 1 = Just True
  fromDouble _ = Nothing

instance (Monoid v, LPVar a v, LPVar b v) => LPVar (a, b) v where
  var (a, b) = var a `mappend` var b

instance (Monoid v, LPVar a v, LPVar b v, LPVar c v) => LPVar (a, b, c) v where
  var (a, b, c) = var a `mappend` var b `mappend` var c

instance LPVar String String where
  var = id

asLinFunc :: (Ord v, Additive c, Num c) => v -> LinFunc v c
asLinFunc v = linCombination [(1, v)]

varLF :: (LPVar a v, Additive c, Num c) => a -> LinFunc v c
varLF = asLinFunc . var

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
