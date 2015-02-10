{-|
Module      : HSat.Problem.BSP.Common.Sign
Description : The Sign data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the data type for a 'Sign', the part
of a 'Literal' denoting whether it is the positive or negative
occurence of a 'Variable'
-}
module HSat.Problem.BSP.Common.Sign (
  -- * Data Type
  Sign(..),
  -- * Constructors
  mkSign,
  mkSignFromInteger,
  -- * Conversions
  signToInteger,
  -- * Queries
  isPos,isNeg,
  -- * Constants
  pos,neg
  ) where

import HSat.Printer
import System.Random

{-|
A Sign describes, when given a corresponding 'Literal', how that 'Literal' appears within
either a BSP or CNF problem. It can either be positive or negative, and is internally represented
as a 'Bool'
-}

newtype Sign = Sign {
  getBool :: Bool -- ^ Internal representation is a 'Bool'
  }
  deriving (Eq,Show)

--Constructors

{-|
A simple constructor for the 'Sign' type when given a 'Bool'
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
Converts an 'Int' to a 'Sign'. Does not work on the value zero, and will throw a runtime error
-}
mkSignFromInteger :: Integer -> Sign
mkSignFromInteger i
  | i < 0 = neg
  | i > 0 = pos
  | otherwise = error "HSat.Problem.BSP.CNF.Common.Sign:fromInteger: Argument zero"

{-|
Converts a 'Sign' to the 'Int' 1 if positive, and the 'Int' -1 if negative.
-}
signToInteger :: Sign -> Integer
signToInteger s
  | isPos s = 1
  | otherwise = -1

{-|
Returns True if the inner Bool is True
-}
isPos :: Sign -> Bool
isPos = getBool

{-|
Returns True if the inner Bool is False
-}
isNeg :: Sign -> Bool
isNeg = not . getBool

instance Printer Sign where
  compact (Sign True) = text "+"
  compact (Sign False) = text "-"
  unicode (Sign True) = green . text $ "+"
  unicode (Sign False) = red . text $ "-"
  noUnicode = compact

instance Ord Sign where
  compare (Sign a) (Sign b) = compare a b

instance Random Sign where
  randomR (l,r) g =
    let l' = getBool l
        r' = getBool r
        (res,g') = randomR (l',r') g
    in (mkSign res,g')
  random g =
    let (res,g') = random g
    in (mkSign res,g')
