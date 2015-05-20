{-|
Module      : HSat.Make.Internal
Description : Internal functions for making problems
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This mdoule exports common functions neded when creating 'Problem's
-}

module HSat.Make.Internal (
  evalBounded,
  Bounds,
  getLesser,
  getGreater,
  mkBounds,
  mkExact,
  mkMinimum,
  mkMaximum,
  mkNoBounds
  ) where

import Control.Monad.Random.Class
import Control.Monad.Random
import HSat.Printer

{-|
Evaluates a 'Bounds' type to a concrete value within a 'MonadRandom' context
-}
evalBounded :: (MonadRandom m, Bounded a, Random a) => Bounds a -> m a
evalBounded (Bounds a b) = getRandomR (a,b)

{-|
A 'Bounds' type is essentially a typle denoting two values of which a random
value will be between (inclusive)
-}
data Bounds a = Bounds a a
  deriving (Eq,Show)

instance (Printer a) => Printer (Bounds a) where
  compact (Bounds a b) = compact a <+> compact b
  noUnicode (Bounds a b) = noUnicode a <+> noUnicode b
  unicode (Bounds a b) = unicode a <+> unicode b

{-|
Takes two values and creates a 'Bounds' out of them. This assures that order
is preserved correctly
-}
mkBounds :: (Ord a, Bounded a) => a -> a -> Bounds a
mkBounds a b
  | a < b = Bounds a b
  | otherwise = Bounds b a

{-|
If we wish to have a value created, but lift it to the Bounds type, we can
use this function
-}
mkExact :: (Ord a, Bounded a) => a -> Bounds a
mkExact a = Bounds a a

{-|
Creates a 'Bounds' type that is bounded by the arbitrary type's maximum
value
-}
mkMinimum :: (Ord a, Bounded a) => a -> Bounds a
mkMinimum a = Bounds a maxBound

{-|
Creates a 'Bounds' type that is boudned by the arbitrary type's minimum value
-}
mkMaximum :: (Ord a, Bounded a) => a -> Bounds a
mkMaximum = Bounds minBound

{-|
If we want a truly random value, it will be created using this
-}
mkNoBounds :: (Ord a, Bounded a) => Bounds a
mkNoBounds = Bounds minBound maxBound

{-|
Gets the lesser of the two bounded values
-}
getLesser :: Bounds a -> a
getLesser (Bounds a _) = a

{-|
Gets the greater of the two bounded values
-}
getGreater :: Bounds a -> a
getGreater (Bounds _ b) = b
