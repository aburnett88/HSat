{-|
Module      : HSat.Problem.BSP.Common.Sign
Description : The Sign data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the common functions and definition of 'Sign' which
represents the part of a 'Literal' that decides whether it is the positive or
negative occurence of the associated 'Variable'
-}
module HSat.Problem.BSP.Common.Sign (
  -- * Data Type
  Sign(..),
  -- * Construct Signs
  mkSign,            -- :: Bool -> Sign
  mkSignFromInteger, -- :: Integer -> Sign
  -- * Conversions
  signToInteger,     -- :: Sign -> Integer
  -- * Tests
  isPos,isNeg,       -- :: Sign -> Bool
  -- * Constants
  pos,neg            -- :: Sign
  ) where

import HSat.Printer
import System.Random

name :: String
name = "HSat.Problem.BSP.Common.Sign"

{-|
A 'Sign' describes, when given an associated 'Variable', how that 'Variable'
should be interpreted as the negative or positive occurence of that 'Variable'

Internally represented as a 'Bool'
-}

newtype Sign = Sign {
  -- | The underlying 'Bool' that is represented
  getBool :: Bool
  }
  deriving (Eq)

instance Show Sign where
  showsPrec = show'

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
Constructs a 'Sign' from an 'Integer'. Positive 'Integer's construct positive
'Sign's and negative 'Integer's negative ones.

When given a zero, this function throws a runtime error. 
-}
mkSignFromInteger   :: Integer -> Sign
mkSignFromInteger i
  | i < 0 = neg
  | i > 0 = pos
  | otherwise = error (name ++ ":mkSignFromInteger: Argument " ++ show i)

{-|
Constructs an 'Integer' from a 'Sign'. Will only return the values 1 or (-1)
depending upon whether the 'Sign' is positive or negative. 
-}
signToInteger      :: Sign -> Integer
signToInteger sign
  | isPos sign =  1
  | otherwise  = -1

{-|
Tests if the 'Sign' is positive
-}
isPos :: Sign -> Bool
isPos = getBool

{-|
Tests if the 'Sign' is negative
-}
isNeg :: Sign -> Bool
isNeg = not . getBool

instance Printer Sign where
  compact (Sign True)  = text "+"
  compact (Sign False) = text "-"
  unicode (Sign True)  = green . text $ "+"
  unicode (Sign False) = red . text $ "-"
  noUnicode            = compact

{-|
Compares the underlying 'Bool' values
-}
instance Ord Sign where
  compare (Sign a) (Sign b) = compare a b

{-|
Gets a random 'Bool', then packs it up in a 'Sign' data type
-}
instance Random Sign where
  randomR (l,r) g =
    let l'       = getBool l
        r'       = getBool r
        (res,g') = randomR (l',r') g
    in (mkSign res,g')
  random g        =
    let (res,g') = random g
    in (mkSign res,g')
