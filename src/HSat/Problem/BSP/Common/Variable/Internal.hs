{-|
Module      : HSat.Data.BSP.Common.Variable.Internal
Description : The Variable data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the definition of 'Variable' which represents the part of
a 'Literal' that denotes its numerical value within a larger problem.
-}
module HSat.Problem.BSP.Common.Variable.Internal (
  Variable(..)
  ) where

import Data.Word
import HSat.Printer

{-|
A 'Variable' represents a numerial representation of a variable wtihin a
problem.

Internally represented as a 'Word'
-}
newtype Variable = Variable {
  -- | The underlying 'Word' that is represented
  getWord :: Word
  } deriving (Eq)

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
