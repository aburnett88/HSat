{-|
Module      : HSat.Data.BSP.Common.Clauses
Description : The Clauses data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

A 'Clauses' data type represents an abstract list of 'Clause's.

Internally this is represented as a Vector. 
-}
module HSat.Data.BSP.Common.Clauses (
  -- * Data Structure
  Clauses(..),
  -- * Constructors
  empty,
  addClause,
  fromList,
  -- * Conversions
  toList,
  -- * General Functions
  length,
  maxVariable,
  isEmpty
  ) where

import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import HSat.Data.BSP.Common.Clause (Clause)
import qualified HSat.Data.BSP.Common.Clause as C hiding (fromList, length, toList)
import Prelude hiding (length)

{-|
This data type is a newtype wrapper around a 'Vector' of 'Clause's. 
-}
newtype Clauses = Clauses {
  -- | The internal 'Vector' of 'Clause's
  _clauses :: Vector Clause
  } deriving (Eq,Show)

{-|
Takes a list of 'Clause's and turns this into a 'Clause's
-}
fromList :: [Clause] -> Clauses
fromList = Clauses . V.fromList

{-|
Takes a 'Clauses' and returns a list of 'Clause's
-}
toList :: Clauses -> [Clause]
toList = V.toList . _clauses

{-|
Counts the number of 'Clause's within a 'Clauses'
-}
length :: Clauses -> Int
length = V.length . _clauses

{-|
Takes a 'Clauses' and a 'Clause' and appends the 'Clause' to the list of 'Clause's inside the 'Clauses' argument
-}
addClause :: Clauses -> Clause -> Clauses
addClause (Clauses cl) c = Clauses $ V.snoc cl c

{-|
Constructs a 'Clauses' with no 'Clause' inside
-}
empty :: Clauses
empty = Clauses $ V.empty

{-|
Finds the maximum 'Word' in each 'Variable' in all 'Clause's. If there are none (or all 'Clause' are empty) then 0 is returned
-}
maxVariable :: Clauses -> Word
maxVariable (Clauses c)
  | c == V.empty = 0
  | otherwise = V.maximum . V.map C.maxVariable  $ c

{-|
Checks if the 'Clauses' argument is empty
-}
isEmpty :: Clauses -> Bool
isEmpty (Clauses cl) = cl == V.empty
