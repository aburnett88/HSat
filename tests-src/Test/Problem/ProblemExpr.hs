{-|
Module      : Test.Problem.ProblemExpr
Description : The ProblemExpr test node and associated tests
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Provides the tests for the ProblemExpr type and its children types
-}

module Test.Problem.ProblemExpr (
  tests
  ) where

import           HSat.Problem.ProblemExpr
import           HSat.Problem.ProblemType
import qualified Test.Problem.BSP.CNF as CNF
import qualified Test.Problem.BSP.Common as Common
import           TestUtils

name :: String
name = "ProblemExpr"

tests :: TestTree
tests =
  testGroup name [
    testGroup "problemType" [
       problemTypeTest1
       ],
    testGroup "mkCNFProblem" [
       mkCNFProblemTest1
       ],
    testGroup "changeProblemType" [
      changeProblemTypeTest1
      ],
    testGroup "problemToCNF" [
      problemToCNFTest1
      ],
    CNF.tests,
    Common.tests
    ]

problemTypeTest1 :: TestTree
problemTypeTest1 =
  testProperty "CNF expr == P.CNF" $ property (
    \cnf ->
    (problemType . mkCNFProblem $ cnf) == (CNF :: ProblemType)
    )

mkCNFProblemTest1 :: TestTree
mkCNFProblemTest1 =
  testProperty "mkCNFProblem cnf == cnf" $ property (
    \cnf ->
    case mkCNFProblem cnf of
      CNFExpr cnf' -> cnf == cnf'
    )

changeProblemTypeTest1 :: TestTree
changeProblemTypeTest1 =
  testProperty "problemType . chnageProblemType p x == p" $ property (
    \(problemExpr,p) ->
    problemType (changeProblemType p problemExpr) == p
    )

problemToCNFTest1 :: TestTree
problemToCNFTest1 =
  testProperty "problemToCNF . mkCNFProblem cnf == cnf" $ property (
    \cnf ->
    (problemToCNF . mkCNFProblem $ cnf) == cnf
    )


