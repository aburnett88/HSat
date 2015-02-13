module HSat.Problem.BSP.CNF.Builder.Internal (
  CNFBuilder(..),
  CNFBuilderError(..)
  ) where

import HSat.Problem.BSP.Common
import HSat.Printer
import Data.Word
import HSat.Validate
import qualified Data.Vector as V
import HSat.Problem.BSP.Common.Clauses.Internal (printClausesWithContext)
import HSat.Problem.BSP.Common.Clause.Internal (printClauseWithContext)

{-|
A 'CNFBuilder' represents a 'CNF' as it is being constructed. This data type
is for when the number of expected 'Clauses' and maximum 'Variable' within
a 'Problem' have been defined beforehand, and this data structure is used to
enforce these constraints on the problem
-}
data CNFBuilder = CNFBuilder {
  -- | The maximum 'Word' within a 'Variable' expected
  getExptdMaxVar :: Word   ,
  -- | The expected number of 'Clauses'
  getExptdClNumb :: Word   ,
  -- | The current number of 'Clauses'
  getCurrClNumb  :: Word   ,
  -- | The current 'Clauses'
  getCurrClauses :: Clauses,
  -- | The current 'Clause' 'Literal's are added to
  getCurrClause :: Clause
  }
  deriving (Eq,Show)

canAddLiteral :: CNFBuilder -> Bool
canAddLiteral builder =
  let curr  = getCurrClNumb builder
      exptd = getExptdClNumb builder
  in (curr  <= exptd)

canFinalise :: CNFBuilder -> Bool
canFinalise builder =
  let curr  = getCurrClNumb builder
      exptd = getExptdClNumb builder
  in (curr  == exptd)

canFinishClause         :: CNFBuilder -> Bool
canFinishClause builder =
  let curr  = getCurrClNumb builder
      exptd = getExptdClNumb builder
  in (curr  <= exptd)

instance Validate CNFBuilder where
  validate (CNFBuilder
            exptdMaxVar
            exptdClNumb
            currClNumb
            currClauses
            currClause) =
    (exptdClNumb >= currClNumb)                        &&
    (V.all testVarInRange $ getVectClause currClauses) &&
    (testVarInRange currClause)                        &&
    (validate currClauses)                             &&
    (validate currClause)
    where
      testVarInRange :: Clause -> Bool
      testVarInRange cl = V.all (varInRange exptdMaxVar) .
                          V.map getVariable $ getVectLiteral $ cl

cTitle,nTitle,uTitle :: String
cTitle               = "CNFBuild"
nTitle               = "CNF Builder"
uTitle               = "CNF Builder"

cMaxVar,nMaxVar,uMaxVar :: String
cMaxVar                 = "Max var"
nMaxVar                 = ""
uMaxVar                 = ""

cEClSize,nEClSize,uEClSize :: String
cEClSize                   = ""
nEClSize                   = ""
uEClSize                   = ""

cCClSize,nCClSize,uCClSize :: String
cCClSize                   = ""
nCClSize                   = ""
uCClSize                   = ""

cClauses,nClauses,uClauses :: String
cClauses                   = ""
nClauses                   = ""
uClauses                   = ""

cClause,nClause,uClause :: String
cClause                 = ""
nClause                 = ""
uClause                 = ""

nOr,uOr :: String
nOr     = "OR"
uOr     = "OR"

nAnd,uAnd :: String
nAnd      = "AND"
uAnd      = "AND"

docClSize                          :: String -> String -> CNFBuilder -> Doc
docClSize strExptd strCurr builder =
  (text strExptd) <> colon               <+>
  (text . show $ getExptdClNumb builder)  <>
  (text strCurr) <> colon                <+>
  (text . show $ getCurrClNumb builder)

docClauses                  :: String -> (Clauses -> Doc) -> CNFBuilder -> Doc
docClauses str func builder =
  (text str)                      <>
  (func $ getCurrClauses builder)

docClause                  :: String -> (Clause -> Doc) -> CNFBuilder -> Doc
docClause str func builder =
  (text str)                     <>
  (func $ getCurrClause builder)

nBoolToQ,uBoolToQ :: Bool -> Doc
nBoolToQ True     = yes
nBoolToQ False    = no
uBoolToQ True     = green yes
uBoolToQ False    = red no

yes,no :: Doc
yes    = text "Yes"
no     = text "No"

printFacts :: CNFBuilder -> (Bool -> Doc) -> Doc
printFacts builder f =
  (text "addLiteral") <> colon <+> f (canAddLiteral builder) <>
  line <>
  (text "canFinalise") <> colon <+> f (canFinalise builder)  <>
  line <>
  (text "canFinishClause") <> colon <+> f (canFinishClause builder)

instance Printer CNFBuilder where
  compact builder =
    (text cTitle)                                        <+>
    docVar cMaxVar builder                               <+>
    docClSize cEClSize cCClSize builder                   <>
    (text "/") <> (text . show $ getExptdClNumb builder)  <>
    line                                                  <>
    docClauses cClauses compact builder                   <>
    line                                                  <>
    docClause cClause compact builder
  noUnicode builder =
    (text nTitle) <>
    docVar nMaxVar builder <>
    docClSize nEClSize nCClSize builder <>
    line <>
    docClauses nClauses (verboseClauses nAnd nOr builder noUnicode) builder <>
    line <>
    docClause nClause (verboseClause nOr builder noUnicode) builder <>
    line <>
    printFacts builder nBoolToQ
  unicode builder =
    (text uTitle) <>
    docVar uMaxVar builder <>
    docClSize uEClSize uCClSize builder <>
    line <>
    docClauses uClauses (verboseClauses uAnd uOr builder unicode) builder <>
    line <>
    docClause uClause (verboseClause uOr builder unicode) builder <>
    line <>
    printFacts builder uBoolToQ

verboseClause :: String -> CNFBuilder -> (Literal -> Doc) -> (Clause -> Doc)
verboseClause str builder func =
  printClauseWithContext str (getExptdMaxVar builder) func

verboseClauses :: String -> String -> CNFBuilder -> (Literal -> Doc) -> (Clauses -> Doc)
verboseClauses strAnd strOr builder func =
  printClausesWithContext strAnd strOr (getExptdMaxVar builder) func

docVar             :: String -> CNFBuilder -> Doc
docVar str builder =
  (text str) <> colon                    <+>
  (text . show $ getExptdMaxVar builder)

{-|
CNFBuilderError describes the errors that can be thrown by a CNFBuidler.
-}
data CNFBuilderError =
  -- | When the incorrect number of 'Clauses' has been defined.
  IncorrectClauseNumber Word Word |
  -- | When a 'Literal' is constructed outside the range specified
  LitOutsideRange Word Word
  deriving (Eq,Show)

instance Validate CNFBuilderError where
  validate (IncorrectClauseNumber gotten expected) =
    (expected /= gotten)
  validate (LitOutsideRange gotten expected)       =
    (expected < gotten) ||
    (gotten == 0)

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
