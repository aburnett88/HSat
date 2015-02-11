{-|
Module      : Test.Problem.BSP.Common.Clause
Description : The test leaf for the Clause type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Clause module
-}

module Test.Problem.BSP.Common.Clause (
  tests
  ) where

import qualified Data.Vector as V
import           HSat.Problem.BSP.Common.Clause
import           TestUtils

name :: String
name = "Clause"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkClause" [
       mkClauseTest1
       ],
    testGroup "emptyClause" [
      emptyClauseTest1
      ],
    testGroup "mkClauseFromLits" [
      mkClauseFromLitsTest1
      ],
    testGroup "mkClauseFromIntegers" [
      mkClauseFromIntegersTest1
      ],
    testGroup "clauseAddLiteral" [
      clauseAddLitTest1
      ],
    testGroup "clauseToIntegers" [
      clauseToIntegersTest1
      ],
    testGroup "clauseIsEmpty" [
      clauseIsEmptyTest1,
      clauseIsEmptyTest2
      ]
    ]

mkClauseTest1 :: TestTree
mkClauseTest1 =
  testProperty ("let getSizeClause == (length cl), getLiterals == cl in" ++
                "mkClause cl") $ property
  (\vectLiterals ->
    let clause        = mkClause vectLiterals
        expectedLits  = vectLiterals
        gottenLits    = getVectLiteral clause
        expectedSize  = toEnum . V.length $ vectLiterals
        gottenSize    = getSizeClause clause
    in (expectedSize  === gottenSize) .&&.
       (expectedLits  === gottenLits)
       )

emptyClauseTest1 :: TestTree
emptyClauseTest1 =
  testCase ("let getClauseSize emptyClause == 0," ++
            "getLiterals emptyClause == 0") $ do
    let clause       = emptyClause
        expectedSize = 0
        gottenSize   = getSizeClause clause
        expectedLits = V.empty
        gottenLits   = getVectLiteral clause        
    assertEqual "Clause size equal" expectedSize gottenSize
    assertEqual "getLiterals == []" expectedLits gottenLits

mkClauseFromLitsTest1 :: TestTree
mkClauseFromLitsTest1 =
  testProperty ("let V.length lits == size cl,"++
                "V.toList cl == lits in cl = mkClauseFromLits lits")
  (\lits ->
    let clause       = mkClauseFromLits lits
        expectedSize = toEnum $ length lits
        gottenLits   = getVectLiteral clause
        expectedLits = V.fromList lits
        gottenSize   = getSizeClause clause
    in (expectedSize === gottenSize) .&&.
       (expectedLits === gottenLits)
       )

clauseToIntegersTest1 :: TestTree
clauseToIntegersTest1 =
  testProperty "mkClauseFromIntegers . clauseToIntegers == id" $ property
  (\clause ->
    let gottenClause  = mkClauseFromIntegers $ clauseToIntegers clause
    in clause         === gottenClause
    )

clauseAddLitTest1 :: TestTree
clauseAddLitTest1 =
  testProperty "addLit c l == mkClause c ++ [l]" $ property
  (\(clause,lit) ->
    let listOfLits     = (V.toList . getVectLiteral $ clause) ++ [lit]
        expectedClause = mkClauseFromLits listOfLits
        gottenClause   = clauseAddLiteral clause lit
    in expectedClause  === gottenClause
       )

mkClauseFromIntegersTest1 :: TestTree
mkClauseFromIntegersTest1 =
  testProperty "clauseToIntegers . mkClauseFromIntegers == id" $
  forAll
  (listOf mkIntegerNonZero)
  (\ints ->
    let gottenInts = clauseToIntegers $ mkClauseFromIntegers ints
    in ints        === gottenInts
       )

clauseIsEmptyTest1 :: TestTree
clauseIsEmptyTest1 =
  testCase "clauseIsEmpty [] == True" $
  assertBool "clauseIsEmpty [] == False" (clauseIsEmpty emptyClause)

clauseIsEmptyTest2 :: TestTree
clauseIsEmptyTest2 =
  testCase "clauseIsEmpty [-1,1] == False" $
  assertBool "clauseIsEmpty [-1,1] == True" (
    not . clauseIsEmpty . mkClauseFromIntegers $ [-1,1]
    )
