{-|
Module      : HSat.Data.BSP.Common.Variable
Description : The Variable data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the data type for a Variable
-}
module HSat.Data.BSP.Common.Variable (
  -- * Data Type
  Variable(..),
  -- * Constructors
  fromInt,
  fromWord,
  fromWord',
  fromInt',
  -- * Deconstructors
  getWord,
  -- * Utility Functions
  varInRange
  ) where

import Data.Word
import HSat.Utils

{-|
The Variable type is a newtype wrapper for a Word
-}
newtype Variable = Variable {
  _getWord :: Word -- ^ The Word representation of the Variable
  } deriving (Eq,Show)

{-|
Constructs a Variable from an Int. Fails if it is a zero.
If it is a negative number, the absolute value is used. 
-}
fromInt :: Int -> Variable
fromInt i
  | i > 0 = Variable . toEnum $ i
  | i < 0 = Variable . toEnum . abs $ i
  | otherwise = error "HSat.CNF.Variable.fromInt: Argument zero"

{-|
Constructs a Variable from an Int without checking the zeroth
case
-}
fromInt' :: Int -> Variable
fromInt' = Variable . toEnum . abs

{-|
Constructs a Variable from a Word. Fails if it is a zero
-}
fromWord :: Word -> Variable
fromWord i
  | i == 0 = error "HSat.CNF.Variable.fromWord: Argument zero"
  | otherwise = Variable i

{-|
Alternative Word constructor. Does not check for a zero, so
therefore can not be used safely with other functions.
-}
fromWord' :: Word -> Variable
fromWord' = Variable

{-|
Returns the underlying Word in the Variable
-}
getWord :: Variable -> Word
getWord = _getWord

{-|
Takes an argument x. Returns True if the underlying variable is within the range 0 < v <= x.
-}
varInRange :: Word -> Variable -> Bool
varInRange range (Variable w) = 0 < w && w <= range
