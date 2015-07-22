{-|
Module      : HSat.Make.Common
Description : Internal functions for making problems
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports common functions needed when creating 'Problem's
-}

module HSat.Make.Common (
  -- * Bounds
  Bounds,
  evalBounded, -- :: (MonadRandom m, Bounded a, Random a) => Bounds a -> m a
  mkBounds   , -- :: (Ord a, Bounded a) => a -> a -> Bounds a
  mkExact    , -- :: (Ord a, Bounded a) => a -> Bounds a
  mkMinimum  , -- :: (Ord a, Bounded a) => a -> Bounds a
  mkMaximum  , -- :: (Ord a, Bounded a) => a -> Bounds a
  mkNoBounds , -- :: (Ord a, Bounded a) => Bounds a
  getLesser  , -- :: Bounded a -> a
  getGreater , -- :: Bounded a -> a
  -- * PosDouble
  PosDouble  ,
  mkPosDouble, -- :: Double -> PosDouble
  getDouble  , -- :: PosDouble -> Double
  ) where

import Control.Monad.Random
import HSat.Printer

{-|
A 'Bounds' type is essentially a tuple denoting two values of which a random
value will be between (inclusive)
-}
data Bounds a = Bounds a a
  deriving (Eq,Show)

instance (Printer a) => Printer (Bounds a) where
  compact (Bounds a b)   = compact a   <+> compact b
  noUnicode (Bounds a b) = noUnicode a <+> noUnicode b
  unicode (Bounds a b)   = unicode a   <+> unicode b

{-|
Evaluates a 'Bounds' type to a concrete value within a 'MonadRandom' context
-}
evalBounded              :: (MonadRandom m, Bounded a, Random a) =>
                            Bounds a -> m a
evalBounded (Bounds a b) = getRandomR (a,b)

{-|
Takes two values and creates a 'Bounds' out of them. This assures that order
is preserved correctly
-}
mkBounds :: (Ord a, Bounded a) => a -> a -> Bounds a
mkBounds a b
  | a < b     = Bounds a b
  | otherwise = Bounds b a

{-|
If we wish to have a value created, but lift it to the Bounds type, we can
use this function
-}
mkExact   :: (Ord a, Bounded a) => a -> Bounds a
mkExact a = Bounds a a

{-|
Creates a 'Bounds' type that is bounded by the arbitrary type's maximum
value
-}
mkMinimum   :: (Ord a, Bounded a) => a -> Bounds a
mkMinimum a = Bounds a maxBound

{-|
Creates a 'Bounds' type that is bounded by the arbitrary type's minimum value
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
getLesser              :: Bounds a -> a
getLesser (Bounds a _) = a

{-|
Gets the greater of the two bounded values
-}
getGreater              :: Bounds a -> a
getGreater (Bounds _ b) = b

instance (Printer a, Printer b) => Printer (Either a b) where
  compact (Left a)    = compact a
  compact (Right a)   = compact a
  noUnicode (Left a)  = noUnicode a
  noUnicode (Right a) = noUnicode a
  unicode (Left a)    = unicode a
  unicode (Right a)   = unicode a
{-|
A new type wrapper around a Double. 
-}
newtype PosDouble = PosDouble Double
  deriving (Eq,Show)

instance Printer PosDouble where
  compact (PosDouble d)   = compact d
  noUnicode (PosDouble d) = noUnicode d
  unicode (PosDouble d)   = unicode d

{-|
The only constructor allows only positive Double's
-}
mkPosDouble :: Double -> PosDouble
mkPosDouble x
  | x < 0.0 = error "mkPosdouble invalid arg"
  | otherwise = PosDouble x

{-|
Gets the Double value from the PosDouble
-}
getDouble               :: PosDouble -> Double
getDouble (PosDouble x) = x

instance Bounded PosDouble where
  minBound = PosDouble 0.0
  maxBound = PosDouble . fromIntegral $ (maxBound :: Word)

instance Random PosDouble where
  randomR (PosDouble l,PosDouble r) g =
    let (p,g') = randomR (l,r) g
    in (PosDouble p,g')
  random                              = randomR (minBound,maxBound)

instance Ord PosDouble where
  compare (PosDouble l) (PosDouble r) = compare l r

instance Functor Bounds where
  fmap f (Bounds a b) = Bounds (f a) (f b)
