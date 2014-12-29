{-|
Module      : HSat.Data.BSP.Common.Sign
Description : The Sign data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the data type for a Sign, the part
of a literal denoting whether it is the positive or negative
occurence of a Variable.
-}
module HSat.Data.BSP.Common.Sign (
  -- * Data Type
  Sign(..),
  -- * Constructors
  fromBool,
  fromInt,
  fromInt',
  -- * Conversions
  toBool,
  -- * Queries
  isPos,isNeg,
  -- * Constants
  var,varNeg
  ) where

import HSat.Utils

{-|
The sign of a literal within a Clause denotes whether it is the positive
or negative occurence of that Variable.
-}
newtype Sign = Sign {
  _getBool :: Bool -- ^ Internal representation is a Boolean
  }
  deriving (Eq,Show)

{-|
This function takes a Bool and converts it to a Sign
-}
fromBool :: Bool -> Sign
fromBool = Sign

{-|
This function takes an Int and constructs the Sign part of the Variable for that Int. Will throw an error if given zero
-}
fromInt       :: Int -> Sign
fromInt i
  | i == 0    = error "HSat.CNF.Sign.fromInt: Argument zero"
  | i < 0     = varNeg
  | otherwise = var

{-|
This function takes an Int and constructs the Sign part of the variable. It does not check for zero
-}
fromInt' :: Int -> Sign
fromInt' i
  | i < 0 = varNeg
  | otherwise = var

{-|
This function converts a Sign to a Bool
-}
toBool :: Sign -> Bool
toBool = _getBool

{-|
Tests whether a Sign is True
-}
isPos :: Sign -> Bool
isPos = _getBool

{-|
Tests whether a Sign is False
-}
isNeg :: Sign -> Bool
isNeg = not . _getBool

{-|
A constant positive occurence of a Sign
-}
var :: Sign
var = Sign True

{-|
A constant negative occurence of a Sign
-}
varNeg :: Sign
varNeg = Sign False

notMinus,notExclam,plus,spaceStr :: String
notMinus = "-"
notExclam = "!"
plus = "+"
spaceStr = " "

--Instances
instance Printer Sign where
  compact (Sign False)  = text notMinus
  compact _             = text plus
  coloured (Sign False) = text notExclam
  coloured _            = text spaceStr
