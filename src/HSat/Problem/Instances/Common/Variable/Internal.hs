{-|
Module      : HSat.Problem.Instances.Common.Variable.Internal
Description : The Variable data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module describes the internal workings of the 'Variable' data type.
-}
module HSat.Problem.Instances.Common.Variable.Internal (
  Variable(..)
  ) where

import HSat.Printer

{-|
A 'Variable' is a numerical representation of a variable within a problem.

Internally it is represented as a 'Word'
-}
newtype Variable = Variable {
  -- | The 'Word' that represents this variable
  getWord :: Word
  } deriving (Eq)

{-
Using the show' function in HSat.Printer to generate a Show instance
-}
instance Show Variable where
  showsPrec = show'

instance Printer Variable where
  compact   = text . show . getWord
  unicode   = compact
  noUnicode = compact

{-|
Order is denoted by the underlying 'Word' in the 'Variable'
-}
instance Ord Variable where
  compare (Variable a) (Variable b) = compare a b
