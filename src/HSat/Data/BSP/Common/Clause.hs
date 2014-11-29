{-|
Module      : HSat.Data.BSP.Common.Clause
Description : The Clause data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the Clause data type and associated functions.

A Clause consists of a abstract list of 'Literal's, which in turn represent either the positive or negative occurence of a 'HSat.Data.BSP.Common.Variable'
-}
module HSat.Data.BSP.Common.Clause (
  -- * Data Type
  Clause(..),
  -- * Construction
  empty,
  addLit,
  fromList,
  -- * Conversion
  toList,
  -- * General Functions
  length,
  checkLitBounds,
  maxVariable,
  isEmpty
  ) where

import HSat.Data.BSP.Common.Literal (Literal)
import qualified HSat.Data.BSP.Common.Literal as L
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Prelude hiding (length)

{-|
A Clause contains a finite number of Literals. Internally
this is represented as a Vector
-}
newtype Clause = Clause {
  -- | The vector of Literals that represents the Clause
  _getLits :: Vector Literal
  } deriving (Eq,Show)

{-|
Creates an empty 'Clause'
-}
empty :: Clause
empty = Clause V.empty

{-|
Takes a 'Clause' and a 'Literal' as arguments and appends
the 'Literal' to the 'Clause'
-}
addLit :: Clause -> Literal -> Clause
addLit c l = Clause $ V.snoc (_getLits c) l

{-|
Converts a list of 'Literal's to a 'Clause'
-}
fromList :: [Literal] -> Clause
fromList = Clause . V.fromList

{-|
Turns a 'Clause' into a list of 'Literal's
-}
toList :: Clause -> [Literal]
toList = V.toList . _getLits

{-|
Returns the 'Word' representation of the size of a 'Clause'
-}
length :: Clause -> Word
length = toEnum . V.length . _getLits

{-|
Takes a 'Word', n,  and a 'Clause' as arguments, and checks whether each 'Word' (within each 'Literal') in the 'Clause' is within the range 1 to n
-}
checkLitBounds :: Word -> Clause -> Bool
checkLitBounds w c = V.all (L.litInRange w) (_getLits c)

{-|
Returns the largest 'Word' within a 'Clause'
-}
maxVariable :: Clause -> Word
maxVariable (Clause c)
  | c == V.empty = 0
  | otherwise = V.maximum . V.map L.getWord $ c

{-|
Checks whether a 'Clause' is empty
-}
isEmpty :: Clause -> Bool
isEmpty (Clause cl) = cl == V.empty
