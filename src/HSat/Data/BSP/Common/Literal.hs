{-|
Module      : HSat.Data.BSP.Common.Literal
Description : The Literal data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

A 'Literal' represents either a 'Variable', or the negation
of that 'Variable'.
-}
module HSat.Data.BSP.Common.Literal (
  -- * Data type
  Literal(..),
  -- * Constructors
  fromInt,
  fromTuple,
  -- * Conversions
  toInt,
  toTuple,
  -- * General Functions
  getBool,
  getWord,
  isPositive,
  isNegative,
  litInRange
  ) where

import Data.Word
import HSat.Data.BSP.Common.Variable (Variable)
import HSat.Data.BSP.Common.Sign (Sign)
import qualified HSat.Data.BSP.Common.Sign as S
import qualified HSat.Data.BSP.Common.Variable as V
import HSat.Utils

{-|
The 'Literal' data type. Abstractly representeda s a pair containing a 'Sign' and a 'Variable'
-}

data Literal = Literal {
  -- | The 'Sign' of the 'Literal'. either positive or negative
  _getSign :: Sign,
  -- | The 'Variable' part of the 'Literal'
  _getVariable :: Variable
  } deriving (Eq,Show)

{-|
Constructs a 'Literal' from an 'Int'
-}
fromInt :: Int -> Literal
fromInt i = Literal b w
  where
    b = S.fromInt i
    w = V.fromInt i

err :: String -> String -> String
err = moduleErr "HSat.Data.BSP.Common.Literal"

{-|
Constructs a 'Literal' from a 'Bool' and a 'Word'
-}
fromTuple :: (Bool,Word) -> Literal
fromTuple (_,0) = error (
  err "fromTuple" "Second argument zero")
fromTuple (b,w) = Literal (S.fromBool b) (V.fromWord w)

{-|
Returns the 'Int' representation of a 'Literal'
-}
toInt :: Literal -> Int
toInt v
  | isNegative v = negate . fromEnum . getWord $ v
  | isPositive v = fromEnum . getWord $ v
  | otherwise = error "toInt"

{-|
Returns a tuple consisting of a 'Bool' and a 'Word'
-}
toTuple :: Literal -> (Bool,Word)
toTuple v
  | isPositive v = (True, getWord v)
  | otherwise = (False, getWord v)

{-|
Returns the 'Word' of the underlying 'Variable'
-}
getWord :: Literal -> Word
getWord = V._getWord . _getVariable

{-|
Returns the 'Bool' part of the underlying 'Variable'
-}
getBool :: Literal -> Bool
getBool = S.toBool . _getSign

{-|
Returns True if the 'Literal' contains the posotive occurence of the 'Variable'
-}
isPositive :: Literal -> Bool
isPositive = S.isPos . _getSign

{-|
Returns True if the 'Literal' contains the negative occurence of the 'Variable'
-}
isNegative :: Literal -> Bool
isNegative = S.isNeg . _getSign

{-|
Checks if the 'Variable' in the 'Literal' is within the range 1 .. n where n is the 'Word' argument
-}
litInRange :: Word -> Literal -> Bool
litInRange w = V.varInRange w . _getVariable


