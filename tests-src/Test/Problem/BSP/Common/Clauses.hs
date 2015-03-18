{-|
Module      : Test.Problen.BSP.Common.Clauses
Description : The Clauses tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Clauses module
-}

module Test.Problem.BSP.Common.Clauses (
  tests,
  Internal.genClauses
  ) where

import qualified Data.Set as S
import qualified Data.Vector as V
import           HSat.Problem.BSP.Common.Clauses
import           HSat.Problem.BSP.Common.Literal
import           HSat.Problem.BSP.Common.Sign
import qualified Test.Problem.BSP.Common.Clauses.Internal as Internal
import           TestUtils

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
      findMaxVarTest1
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
  (\vectOfClause ->
    let clauses      = mkClauses vectOfClause
        exptdSize    = toEnum $ V.length vectOfClause
        valSize      = getSizeClauses clauses
        exptdClauses = vectOfClause
        valClauses   = getVectClause clauses
    in (exptdSize === valSize) .&&.
       (exptdClauses === valClauses)
  )

emptyClausesTest1 :: TestTree
emptyClausesTest1 =
  testCase ("let numbOfClauses emptyClause = 0,"++
            "getVectClause = V.empty in emptyClause") $ do
    let exptdSize   = 0
        valSize     = getSizeClauses emptyClauses
        exptdVector = V.empty
        valVector   = getVectClause emptyClauses
    assertEqual
      "Size not zero"
      exptdSize valSize
    assertEqual
      "Vector not empty"
      exptdVector
      valVector

mkClausesFromClauseTest1 :: TestTree
mkClausesFromClauseTest1 =
  testProperty "toList . getvect . mkClausesFromClause c == c" $ property
  (\clauses ->
    let val = V.toList . getVectClause $ mkClausesFromClause clauses
    in clauses === val
  )

clausesAddClauseTest1 :: TestTree
clausesAddClauseTest1 =
  testProperty "clausesAddClause cl c == cl ++c" $ property
  (\(clauses,clause) ->
    let exptd = mkClausesFromClause . V.toList $
                V.snoc (getVectClause clauses) clause
        val   = clausesAddClause clauses clause
    in exptd === val
  )

mkClausesFromIntegersTest1 :: TestTree
mkClausesFromIntegersTest1 =
  testProperty "clausesToIntegers . mkClausesFromIntegers == id" $
  forAll
  (listOf $ listOf mkIntegerNonZero)
  (\ints ->
    let exptd = clausesToIntegers $ mkClausesFromIntegers ints
    in exptd === ints
  )

clausesToIntegersTest1 :: TestTree
clausesToIntegersTest1 =
  testProperty "mkClausesFromIntgers . clausesToIntegers == id" $ property
  (\clauses ->
    let exptd = mkClausesFromIntegers $ clausesToIntegers clauses
    in  exptd === clauses
  )

clausesIsEmptyTest1 :: TestTree
clausesIsEmptyTest1 =
  testCase "clausesIsEmpty []" $
  assertBool "clausesIsEmpty [] == False" (clausesIsEmpty emptyClauses)

{-
Test the non empty versions
-}
clausesIsEmptyTest2 :: TestTree
clausesIsEmptyTest2 =
  testProperty "clausesIsEmpty [..] == False" $
  forAll
  (do
      clauses <- arbitrary
      clause <- arbitrary
      return $ clausesAddClause clauses clause
  )
  (not . clausesIsEmpty)

{-
We test the element where there are no literals. In this instance, the maxVar
returned should be zero. 
-}
findMaxVarTest1 :: TestTree
findMaxVarTest1 =
  testProperty "maxVar clauses == maximum . toList $ clauses" $ property
  (\clauses ->
    let litList = map abs . concat $ clausesToIntegers clauses
        exptd   = if null litList then
                  0 else
                  maximum litList
        val     = toInteger $ findMaxVar clauses
    in exptd === val
  )

getSetOfVarsTest1 :: TestTree
getSetOfVarsTest1 =
  testProperty "getSetOfVars == S.fromList $ concat lits" $ property
  (\litlist ->
    let exptd = S.fromList . map getVariable $ concat litlist
        val   = getSetOfVars . mkClausesFromIntegers $
                      map (map literalToInteger) litlist
    in exptd  === val
  )

getSetPosTest1 :: TestTree
getSetPosTest1 =
  testProperty "getSetPos = S.fromList (filter isPos) $ concat lits" $ property
  (\litlist ->
    let exptd = S.fromList . map getVariable .
                      filter (isPos . getSign) $ concat litlist
        val   = getSetPos . mkClausesFromIntegers $
                      map (map literalToInteger) litlist
    in exptd  === val
  )

getSetNegTest1 :: TestTree
getSetNegTest1 =
  testProperty "getSetNeg = S.fromList (filter isNeg) $ concat lits" $ property
  (\litlist ->
    let exptd = S.fromList . map getVariable .
                      filter (isNeg . getSign) $ concat litlist
        val   = getSetNeg . mkClausesFromIntegers $
                      map (map literalToInteger) litlist
    in exptd  === val
  )
