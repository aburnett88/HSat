{-|
Module      : Test.Problen.BSP.Common.Clauses
Description : The 'Clause' test leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Clauses module
-}

module Test.Problem.BSP.Common.Clauses (
  tests
  ) where

import           Control.Monad (replicateM)
import qualified Data.Set as S
import qualified Data.Vector as V
import           HSat.Problem.BSP.Common.Clause (emptyClause)
import           HSat.Problem.BSP.Common.Clauses
import           HSat.Problem.BSP.Common.Literal
import           HSat.Problem.BSP.Common.Sign
import           HSat.Problem.BSP.Common.Variable
import qualified Test.Problem.BSP.Common.Clauses.Internal as Internal
import           TestUtils
import Debug.Trace

name :: String
name = "Clauses"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    testGroup "mkClauses" [
       mkClausesTest1
       ],
    testGroup "emptyClauses" [
       emptyClausesTest1
       ],
    testGroup "mkClausesFromClause" [
       mkClausesFromClauseTest1
       ],
    testGroup "clausesAddClause" [
       clausesAddClauseTest1
       ],
    testGroup "mkClausesFromIntegers" [
       mkClausesFromIntegersTest1
       ],
    testGroup "clausesToIntegers" [
       clausesToIntegersTest1
       ],
    testGroup "clausesIsEmpty" [
       clausesIsEmptyTest1,
       clausesIsEmptyTest2
       ],
    testGroup "findMaxVar" [
      findMaxVarTest1,
      findMaxVarTest2
      ],
    testGroup "getSetOfVars" [
      getSetOfVarsTest1
      ],
    testGroup "getSetPos" [
      getSetPosTest1
      ],
    testGroup "getSetNeg" [
      getSetNegTest1
      ]
    ]

mkClausesTest1 :: TestTree
mkClausesTest1 =
  testProperty ("getSizeClauses = V.length cl,"++
                "getVectClause = cl in" ++
                "mkClause cl") $ property
  (\vectClause ->
    let newClause       = mkClauses vectClause
        expectedSize    = toEnum $ V.length vectClause
        gottenSize      = getSizeClauses newClause
        expectedClauses = vectClause
        gottenClauses   = getVectClause newClause
    in (expectedSize    === gottenSize) .&&.
       (expectedClauses === gottenClauses)
  )

emptyClausesTest1 :: TestTree
emptyClausesTest1 =
  testCase ("let numbOfClauses emptyClause = 0,"++
            "getVectClause = V.empty in emptyClause") $ do
    let expectedSize = 0
        gottenSize   = getSizeClauses emptyClauses
        expectedVect = V.empty
        gottenVect   = getVectClause emptyClauses
    assertEqual "Size not zero" expectedSize gottenSize
    assertEqual "Vector not empty" expectedVect gottenVect

mkClausesFromClauseTest1 :: TestTree
mkClausesFromClauseTest1 =
  testProperty "toList . getvect . mkClausesFromClause c == c" $ property
  (\cl ->
    let gottenValue = V.toList . getVectClause $ mkClausesFromClause cl
    in cl           === gottenValue
  )

clausesAddClauseTest1 :: TestTree
clausesAddClauseTest1 =
  testProperty "clausesAddClause cl c == cl ++c" $ property
  (\(clauses,clause) ->
    let expectedValue = mkClausesFromClause $ (
          V.toList . getVectClause $ clauses) ++ [clause]
        gottenValue   = clausesAddClause clauses clause
    in expectedValue  === gottenValue
  )

mkClausesFromIntegersTest1 :: TestTree
mkClausesFromIntegersTest1 =
  testProperty "clausesToIntegers . mkClausesFromIntegers == id" $
  forAll
  (listOf $ listOf mkIntegerNonZero)
  (\ints ->
    let expectedVal = clausesToIntegers $ mkClausesFromIntegers ints
    in expectedVal  === ints
  )

clausesToIntegersTest1 :: TestTree
clausesToIntegersTest1 =
  testProperty "mkClausesFromIntgers . clausesToIntegers == id" $ property
  (\clauses ->
    let expectedVal = mkClausesFromIntegers $ clausesToIntegers clauses
    in  expectedVal === clauses
  )

clausesIsEmptyTest1 :: TestTree
clausesIsEmptyTest1 =
  testCase "clausesIsEmpty []" $
  assertBool "clausesIsEmpty [] == False" (clausesIsEmpty emptyClauses)

clausesIsEmptyTest2 :: TestTree
clausesIsEmptyTest2 =
  testCase "clausesIsEmpty [..]" $
  assertBool "clausesIsEmpty [..] == True" (
    not . clausesIsEmpty $ mkClausesFromClause [emptyClause])

findMaxVarTest1 :: TestTree
findMaxVarTest1 =
  testCase "findMaxVar [] == 0" $
  assertEqual "findMaxVar [] != 0" (findMaxVar emptyClauses) 0
  
findMaxVarTest2 :: TestTree
findMaxVarTest2 =
  testProperty "max litlist == findmaxVar . toClauses $ litlist" $ property
  (\litlist ->
    let expectedVal =
          if null litlist then
            0 else
            if null $ concat litlist then
              0 else
              maximum . map (getWord . getVariable) $ concat litlist
        actualVal   = findMaxVar . mkClausesFromIntegers $
                      map (map literalToInteger) litlist
    in expectedVal  === actualVal
  )

getSetOfVarsTest1 :: TestTree
getSetOfVarsTest1 =
  testProperty "getSetOfVars == S.fromList $ concat lits" $ property
  (\litlist ->
    let expectedVal = S.fromList . map getVariable $ concat litlist
        actualVal   = getSetOfVars . mkClausesFromIntegers $
                      map (map literalToInteger) litlist
    in expectedVal  === actualVal
  )

getSetPosTest1 :: TestTree
getSetPosTest1 =
  testProperty "getSetPos = S.fromList (filter isPos) $ concat lits" $ property
  (\litlist ->
    let expectedVal = S.fromList . map getVariable .
                      filter (isPos . getSign) $ concat litlist
        actualVal   = getSetPos . mkClausesFromIntegers $
                      map (map literalToInteger) litlist
    in expectedVal  === actualVal
  )

getSetNegTest1 :: TestTree
getSetNegTest1 =
  testProperty "getSetNeg = S.fromList (filter isNeg) $ concat lits" $ property
  (\litlist ->
    let expectedVal = S.fromList . map getVariable .
                      filter (isNeg . getSign) $ concat litlist
        actualVal   = getSetNeg . mkClausesFromIntegers $
                      map (map literalToInteger) litlist
    in expectedVal  === actualVal
  )
  
