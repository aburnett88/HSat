module Test.Problem.BSP.CNF (
  tests
  ) where

import           Control.Monad (replicateM)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.Common
import qualified Test.Problem.BSP.CNF.Builder as CNFBuilder
import qualified Test.Problem.BSP.CNF.Internal as Internal
import           TestUtils

name :: String
name = "CNF"

tests :: TestTree
tests = testGroup name [
  Internal.tests,
  CNFBuilder.tests,
  testGroup "mkCNFFromClauses" [
     mkCNFFromClausesTest1
     ],
  testGroup "cnfToIntegers" [
     cnfToIntegersTest1
     ],
  testGroup "mkCNFFromIntegers" [
    mkCNFFromIntegersTest1
    ]
  ]

mkCNFFromClausesTest1 :: TestTree
mkCNFFromClausesTest1 =
  testProperty "mkCNFFromClauses has correct values" $ property
  (\clauses ->
    let gottenClauses = getClauses $ mkCNFFromClauses cnf
    in gottenClauses === clauses
  )

cnfToIntegersTest1 :: TestTree
cnfToIntegersTest1 =
  testProperty "mkCNFFromInegers . cnfToIntegers cnf == cnf" $ property
  (\cnf ->
    let cnf'               = mkCNFFromIntegers $ cnfToIntegers cnf
        expectedClauseNumb = getClauseNumb cnf
        gottenClauseNumb   = getClauseNumb cnf'
        expectedClauses    = getClauses cnf
        gottenClauses      = getClauses cnf'
        expectedVarNumb    = getMaxVar cnf
        gottenVarNumb      = getMaxVar cnf'
    in (epxectedClauseNumb === gottenClauseNumb) .&&.
       (expectedClauses    === gottenClauses   ) .&&.
       (expectedVarNumb    >=  gottenVarNumb   )
  )

mkCNFFromIntegersTest1 :: TestTree
mkCNFFromIntegersTest1 =
  testProperty "cnfToIntegers . mkCNFFromIntegers $ ints == ints" $
  forAll
  (genList $ genList mkIntegerNonZero)
  (\ints ->
    ints === (cnfToIntegers $ mkCNFFromIntegers ints)
    )


