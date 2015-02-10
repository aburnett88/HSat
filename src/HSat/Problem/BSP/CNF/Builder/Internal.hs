module HSat.Problem.BSP.CNF.Builder.Internal (
  CNFBuilder(..),
  CNFBuilderError(..)
  ) where

import HSat.Problem.BSP.Common
import HSat.Printer
import Data.Word

data CNFBuilder = CNFBuilder {
  varNumb :: Word,
  expectedClauseNumb :: Word,
  currentClauseNumb :: Word,
  clauses :: Clauses,
  currentClause :: Clause
  }
  deriving (Eq,Show)

data CNFBuilderError =
  IncorrectClauseNumber Word Word |
  LitOutsideRange Word Word
  deriving (Eq,Show)

unicodeLe,noUnicodeLe :: String
unicodeLe = "<="
noUnicodeLe = "<="

bf :: Doc
bf = text $ "CNF Build Error:\t"

msg1,msg2 :: Doc
msg1 = text "Incorrect Clause number\t"
msg2 = text "Literal outside range\t"
 
instance Printer CNFBuilderError where
  compact (IncorrectClauseNumber gotten expected) =
    text ("ERR - Clause size expected " ++ show expected ++
     "but got " ++ show gotten)
  compact (LitOutsideRange gotten expected) =
    text ("ERR - Literal outside range 0 < " ++ show gotten ++
          " <= " ++ show expected)
  noUnicode (IncorrectClauseNumber gotten expected) =
    bf <+>
    msg1 <>
    (text $ "Expected: " ++ show expected ++
     " Got: " ++ show gotten)
  noUnicode (LitOutsideRange gotten expected) =
    verboseLitOutsideRange noUnicodeLe gotten expected
  unicode i@(IncorrectClauseNumber _ _) =
    red $ noUnicode i
  unicode (LitOutsideRange gotten expected) =
    red $ verboseLitOutsideRange unicodeLe gotten expected
    
verboseLitOutsideRange :: String -> Word -> Word -> Doc
verboseLitOutsideRange lessThan gotten expected =
  bf <+>
  msg2 <>
  text ("0 <" ++ show gotten ++
   lessThan ++ " " ++ show expected)
