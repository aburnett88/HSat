{-|
Module      : Test.Problem.Instances.Common.Clause
Description : The Clause tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Clause module
-}

module Test.Problem.Instances.Common.Clause (
  tests,             -- :: TestTree
  Internal.genClause -- :: Word -> Int -> Gen Clause
  ) where

import           Control.Applicative
import qualified Data.Vector                                   as V
import           HSat.Problem.Instances.Common.Clause
import qualified Test.Problem.Instances.Common.Clause.Internal as Internal
import           TestUtils

name :: String
name = "Clause"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
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
  testProperty ("let getSizeClause " `equiv` " (length cl), getLiterals " `equiv` " cl in" ++
                "mkClause cl") $ property
  (\vectLiterals ->
    let clause    = mkClause $ V.fromList vectLiterals
        valLits   = V.toList $ getVectLiteral clause
        exptdSize = toEnum $ length vectLiterals
        valSize   = getSizeClause clause
    in (valSize === exptdSize) .&&.
       (valLits === vectLiterals)
  )

emptyClauseTest1 :: TestTree
emptyClauseTest1 =
  testCase ("let getClauseSize emptyClause " `equiv` " 0," ++
            "getLiterals emptyClause " `equiv` " []") $ do
    let clause    = emptyClause
        exptdSize = 0
        valSize   = getSizeClause clause
        exptdLits = V.empty
        valLits   = getVectLiteral clause
    assertEqual
      "Clause size not equal"
      exptdSize
      valSize
    assertEqual
      "Literal vector not equal"
      exptdLits
      valLits

{-
Checks that the size and the elements contained are as expected
-}
mkClauseFromLitsTest1 :: TestTree
mkClauseFromLitsTest1 =
  testProperty ("let V.length lits " `equiv` " size cl,"++
                "V.toList cl " `equiv` " lits in cl = mkClauseFromLits lits")
  (\lits ->
    let clause    = mkClauseFromLits lits
        exptdSize = toEnum $ length lits
        exptdLits = V.fromList lits
        valSize   = getSizeClause clause
        valLits   = getVectLiteral clause
    in (exptdSize === valSize) .&&.
       (exptdLits === valLits)
  )

clauseToIntegersTest1 :: TestTree
clauseToIntegersTest1 =
  testProperty ("mkClauseFromIntegers . clauseToIntegers " `equiv` " id") $ property
  (\clause ->
    let val = mkClauseFromIntegers $ clauseToIntegers clause
    in clause === val
  )

clauseAddLitTest1 :: TestTree
clauseAddLitTest1 =
  testProperty ("addLit c l " `equiv` " mkClause c ++ [l]") $ property
  (\(clause,lit) ->
    let litList     = V.toList $ V.snoc (getVectLiteral clause) lit
        exptdClause = mkClauseFromLits litList
        valClause   = clauseAddLiteral clause lit
    in exptdClause === valClause
  )

mkClauseFromIntegersTest1 :: TestTree
mkClauseFromIntegersTest1 =
  testProperty ("clauseToIntegers . mkClauseFromIntegers " `equiv` " id") $
  forAll
  (listOf mkIntegerNonZero)
  (\ints ->
    let valInts = clauseToIntegers $ mkClauseFromIntegers ints
    in ints === valInts
  )

clauseIsEmptyTest1 :: TestTree
clauseIsEmptyTest1 =
  testCase ("clauseIsEmpty [] " `equiv` " True") $
  (clauseIsEmpty emptyClause) @=? True

{-
The property creates a non-empty Clause, and tests that clauseIsEmpty
always returns False. 
-}
clauseIsEmptyTest2 :: TestTree
clauseIsEmptyTest2 =
  testProperty ("clauseIsEmpty [nonempty] " `equiv` " False") $
  forAll
  (liftA2 clauseAddLiteral arbitrary arbitrary)
  (not . clauseIsEmpty)
