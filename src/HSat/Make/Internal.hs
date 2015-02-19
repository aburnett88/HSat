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

evalBounded :: (MonadRandom m, Bounded a, Random a) => Bounds a -> m a
evalBounded (Bounds a b) = getRandomR (a,b)

data Bounds a = Bounds a a
  deriving (Eq,Show)

mkBounds :: (Ord a, Bounded a) => a -> a -> Bounds a
mkBounds a b
  | a < b = Bounds a b
  | otherwise = Bounds b a

mkExact :: (Ord a, Bounded a) => a -> Bounds a
mkExact a = Bounds a a

mkMinimum :: (Ord a, Bounded a) => a -> Bounds a
mkMinimum a = Bounds a maxBound

mkMaximum :: (Ord a, Bounded a) => a -> Bounds a
mkMaximum = Bounds minBound

mkNoBounds :: (Ord a, Bounded a) => Bounds a
mkNoBounds = Bounds minBound maxBound

getLesser :: Bounds a -> a
getLesser (Bounds a b) = a

getGreater :: Bounds a -> a
getGreater (Bounds a b) = b
