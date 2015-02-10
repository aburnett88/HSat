{-|
Module      : HSat.Data.BSP.Common.Literal
Description : The Literal data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the common functions and definition of 'Literal' which
represents the pair of a 'Sign' and a 'Variable' commonly used in Boolean
formulae.
-}
module HSat.Problem.BSP.Common.Literal (
  -- * Data type
  Literal(..),
  -- * Constructors
  mkLiteral,           -- :: Sign -> Variable -> Literal
  mkLiteralFromInteger,-- :: Integer -> Literal
  -- * Conversions
  literalToInteger     -- :: Literal -> Integer
  ) where

import HSat.Printer
import HSat.Problem.BSP.Common.Sign
import HSat.Problem.BSP.Common.Variable

{-|
A 'Literal' describes the pairing of a 'Sign' and 'Variable' and can be used
to either describe the occurence, or the negation of a 'Variable' within a
Boolean formulae. 

Internally represented as a 'Sign' and a 'Variable'
-}

data Literal = Literal {
  -- | The underlying 'Sign' that is represented
  getSign     :: Sign    ,
  -- | The underlying 'Variable' that is represented
  getVariable :: Variable
  } deriving (Eq,Show)

{-|
Constructs a 'Literal' from a 'Sign' and 'Variable'
-}
mkLiteral :: Sign -> Variable -> Literal
mkLiteral = Literal

{-|
Constructs a 'Literal' from an 'Integer'.

It is possible that this may throw an error if the 'Integer' is outside the
range allowed by the underlying representation within 'Variable'
-}
mkLiteralFromInteger         :: Integer -> Literal
mkLiteralFromInteger integer =
  Literal (mkSignFromInteger integer) (mkVariableFromInteger integer)

{-|
Constructs an 'Integer' from a 'Literal'
-}
literalToInteger                    :: Literal -> Integer
literalToInteger (Literal sign var) =
  signToInteger sign * variableToInteger var

instance Printer Literal where
  compact   = text . show . literalToInteger
  noUnicode = compact
  unicode (Literal s v)
    | isPos s = green . unicode $ v
    | otherwise = red . unicode $ v

{-|
Ordered by the 'Variable' and, if they are the same, then by the 'Sign'
-}
instance Ord Literal where
  compare (Literal s v) (Literal s' v') =
    case compare v v' of
      EQ -> compare s s'
      a -> a
