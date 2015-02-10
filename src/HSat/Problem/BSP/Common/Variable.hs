{-|
Module      : HSat.Data.BSP.Common.Variable
Description : The Variable data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the data type for a Variable
-}
module HSat.Problem.BSP.Common.Variable (
  -- * Data Type
  Variable(..),
  -- * Constructors
  mkVariable,
  mkVariableFromInteger,
  -- * Utility Functions
  varInRange,
  variableToInteger
  ) where

import Data.Word
import HSat.Printer

{-|
The Variable type is a newtype wrapper for a Word
-}
newtype Variable = Variable {
  getWord :: Word -- ^ The Word representation of the Variable
  } deriving (Eq,Show)
{-|
A quick constructor for a 'Variable'
-}
mkVariable   :: Word -> Variable
mkVariable 0 = error "HSat.Problem.BSP.Common.Variable:mkVariable: Argument zero"
mkVariable w = Variable w

--The maximum Integer represenation of a Word. Its put through mkVariable so that
--no errors are thrown
maxVariable :: Integer
maxVariable = toInteger . getWord . mkVariable $ maxBound

--A message builder helper
filename     :: String -> String
filename msg = "HSat.Problem.BSP.Common.Variable." ++ msg

--A method of showing the message for bad Integer values 
mkVariableFromIntegerMsg2   :: Integer -> String
mkVariableFromIntegerMsg2 i = filename (
  "mkVariableFromInteger: Cannot construct Variable. Input "++ show i ++
  ", but ranges are from "++(show . negate $ maxVariable) ++ " to " ++
  show maxVariable)

{-|
Constructs a Variable from an Int. Fails if it is a zero.
If it is a negative number, the absolute value is used. 
-}
mkVariableFromInteger :: Integer -> Variable
mkVariableFromInteger i =
  case compare i 0 of
    EQ -> error (filename "mkVariableFromInteger: Argument zero")
    LT ->
      let i' = abs i
      in case compare i' maxVariable of
        GT -> error (mkVariableFromIntegerMsg2 i)
        _ -> Variable . fromInteger $ i'
    GT ->
      case compare i maxVariable of
        GT -> error (mkVariableFromIntegerMsg2 i)
        _ -> Variable . fromInteger $ i

{-|
Takes an argument x. Returns True if the underlying variable is within the range 0 < v <= x.
-}
varInRange                    :: Word -> Variable -> Bool
varInRange range (Variable w) = w <= range

{-|
Takes a 'Variable' and constructs the 'Integer' representation
-}
variableToInteger              :: Variable -> Integer
variableToInteger (Variable w) = toInteger w

instance Printer Variable where
  compact   = text . show . variableToInteger
  unicode   = compact
  noUnicode = compact

instance Ord Variable where
  compare (Variable a) (Variable b) = compare a b
