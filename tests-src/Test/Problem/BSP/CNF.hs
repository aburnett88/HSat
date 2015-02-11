module Test.Problem.BSP.CNF (
  tests
  ) where

import           Control.Monad (replicateM)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.Common
import qualified Test.Problem.BSP.CNF.Builder as CNFBuilder
import           TestUtils

name :: String
name = "CNF"

tests :: TestTree
tests = testGroup name [
  testGroup "mkCNFFromClauses" [
     mkCNFFromClausesTest1
     ],
  testGroup "cnfToIntegers" [
     cnfToIntegersTest1
     ],
  testGroup "mkCNFFromIntegers" [
    mkCNFFromIntegersTest1
    ],
  CNFBuilder.tests
  ]

mkCNFFromClausesTest1 :: TestTree
mkCNFFromClausesTest1 =
  testProperty "mkCNFFromClauses has correct values" $ property (
  \clauses ->
  -- First get a list of vectors of words of the CNF
  let words =  V.toList . V.map (V.map (getWord . getVariable)) .
               V.map getVectLiteral . getVectClause $ clauses :: [V.Vector Word]
      --Find the length of the list
      l = toEnum $ length words
      --Find the max variable
      maxV = (\list -> if list==V.empty then 0 else V.maximum list) $ V.concat words :: Word
      cnf = mkCNFFromClauses clauses
  in
   --Compare these values to make sure this is correct
   (maxV == getNoVars cnf) &&
   (l == getNoClauses cnf) &&
   (clauses == getClauses cnf)
   )

cnfToIntegersTest1 :: TestTree
cnfToIntegersTest1 =
  testProperty "mkCNFFromInegers . cnfToIntegers cnf == cnf" $ property (
    \cnf ->
    cnf == (mkCNFFromIntegers . cnfToIntegers $ cnf)
    )

mkCNFFromIntegersTest1 :: TestTree
mkCNFFromIntegersTest1 =
  testProperty "cnfToIntegers . mkCNFFromIntegers $ ints == ints" $
  forAll
  (choose (0,testMaxClausesSize) >>= flip replicateM (
      choose (0,testMaxClauseSize) >>= flip replicateM mkIntegerNonZero)
   )
  (\ints ->
    ints == (cnfToIntegers . mkCNFFromIntegers $ ints)
    )


