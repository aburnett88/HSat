{-|
Module      : HSat.Problem.BSP.Common.Sign
Description : The Sign type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The 'Sign' datatype is a binary datatype whose main purpose is to denote
a positive or negative occourence of an accompanying data type.

For example, to represent either the positive or negative variable's within
a Boolean Satisfiability Formula. 
-}
module HSat.Problem.Instances.Common.Sign (
  -- * Sign
  Sign(..),
  -- * Construction
  mkSign,            -- :: Bool    -> Sign
  mkSignFromInteger, -- :: Integer -> Sign
  -- * Conversion
  signToInteger,     -- :: Sign -> Integer
  -- * Common Functions
  isPos,isNeg,       -- :: Sign -> Bool
  -- * Constants
  pos,neg            -- :: Sign
  ) where

import Data.Bifunctor
import HSat.Printer
import System.Random

{-|
A 'Sign' is, in its most simple form, a simple binary data type. Internally
it is represented as a 'Bool' value.
-}

newtype Sign = Sign {
  -- | The internally represented 'Bool'
  getBool :: Bool
  }
  deriving (Eq)

{-
Compares the underlying 'Bool' values
-}
instance Ord Sign where
  compare (Sign bool1) (Sign bool2) =
    compare bool1 bool2

{-
Gets a random 'Bool', then packs it up into a Sign type
-}
instance Random Sign where
  randomR signs gen =
    let bools = bimap getBool getBool signs
    in first mkSign $ randomR bools gen
  random gen        =
    first mkSign $ random gen

instance Show Sign where
  showsPrec = show'

instance Printer Sign where
  compact (Sign True)  =         text "+"
  compact (Sign False) =         text "-"
  unicode (Sign True)  = green $ text "+"
  unicode (Sign False) = red   $ text "-"
  noUnicode            = compact

{-|
Constructs a 'Sign' from a 'Bool'
-}
mkSign :: Bool -> Sign
mkSign = Sign

{-|
A static value representing a positive 'Sign'
-}
pos :: Sign
pos = Sign True

{-|
A static value representing a negative 'Sign'
-}
neg :: Sign
neg = Sign False

{-|
Constructs a 'Sign' from an 'Integer'.

A positive 'Integer' will construct a positive 'Sign' while a negative
'Integer' will construct a negative 'Sign'.

An 'Integer' of zero will throw a run-time error. 
-}
mkSignFromInteger       :: Integer -> Sign
mkSignFromInteger intgr
  | intgr < 0 = neg
  | intgr > 0 = pos
  | otherwise = error (name ++ ":" ++ func ++ ": Argument " ++ show intgr)
  where
    name = "HSat.Problem.BSP.Common.Sign"
    func = "mkSignFromInteger"

{-|
Converts a 'Sign' into an 'Integer'.

Will return the value '1' if the 'Sign' is positive and the value '-1' if the
'Sign' is negative.
-}
signToInteger      :: Sign -> Integer
signToInteger sign = if isPos sign then
                       1 else
                       -1
{-|
Returns 'True' if the 'Sign' is positive. 
-}
isPos :: Sign -> Bool
isPos = getBool

{-|
Returns 'True' if the 'Sign' is negative
-}
isNeg :: Sign -> Bool
isNeg = not . getBool

