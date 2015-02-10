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
module HSat.Problem.BSP.Common.Literal (
  -- * Data type
  Literal(..),
  mkLiteral,
  mkLiteralFromInteger,
  literalToInteger
  ) where

import HSat.Problem.BSP.Common.Sign
import HSat.Printer
import HSat.Problem.BSP.Common.Variable

{-|
Tche 'Literal' data type. Abstractly representeda s a pair containing a 'Sign' and a
'Variable'
-}

data Literal = Literal {
  -- | The 'Sign' of the 'Literal'. either positive or negative
  getSign :: Sign,
  -- | The 'Variable' part of the 'Literal'
  getVariable :: Variable
  } deriving (Eq,Show)

{-|
Constructs a 'Literal' from a 'Sign' and 'Variable'
-}
mkLiteral :: Sign -> Variable -> Literal
mkLiteral = Literal

{-|
Constructs a 'Literal' from an 'Integer' representation
-}
mkLiteralFromInteger   :: Integer -> Literal
mkLiteralFromInteger i = Literal (mkSignFromInteger i) (mkVariableFromInteger i)

{-|
Converts a 'Literal' to its 'Integer' representation
-}
literalToInteger               :: Literal -> Integer
literalToInteger (Literal s v) = signToInteger s * variableToInteger v

instance Printer Literal where
  compact   = text . show . literalToInteger
  noUnicode = compact
  unicode (Literal s v)
    | isPos s = green . unicode $ v
    | otherwise = red . unicode $ v

instance Ord Literal where
  compare (Literal s v) (Literal s' v') =
    case compare v v' of
      EQ -> compare s s'
      a -> a
