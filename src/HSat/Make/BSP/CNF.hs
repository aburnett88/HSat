module HSat.Make.BSP.CNF (
  makeCNF
  ) where

import Data.Word
import HSat.Make.BSP.Common.Literal
import HSat.Problem.BSP.CNF

makeCNF :: Word -> [Word] -> Bool -> Bool -> m (Either LiteralMakeError CNF)
makeCNF = undefined
