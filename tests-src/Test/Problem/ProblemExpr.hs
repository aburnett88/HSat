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
import           TestUtils
import TestUtils.Validate
import Control.Applicative
import Test.Problem.ProblemType ()
import qualified Test.Problem.BSP as BSP
import HSat.Problem.ProblemExpr.Internal

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
    BSP.tests
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
      BoolExprs (CNFExpr cnf') -> cnf === cnf'
      _ -> counterexample "Incorrect type, not CNF" False
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

instance Validate ProblemExpr where
  validate (BoolExprs b) = validate b

instance Validate BoolExpr where
  validate (BSPExpr b) = validate b
  validate (CNFExpr cnf) = validate cnf

instance Arbitrary ProblemExpr where
  arbitrary = oneof [
    liftA mkBoolExprProblem arbitrary
    ]
  shrink (BoolExprs b) =
    map mkBoolExprProblem $ shrink b

instance Arbitrary BoolExpr where
  arbitrary = oneof [
    liftA mkBSPBoolExpr arbitrary,
    liftA mkCNFBoolExpr arbitrary
    ]
  shrink (BSPExpr bsp) =
    map mkBSPBoolExpr $ shrink bsp
  shrink (CNFExpr cnf) =
    map mkCNFBoolExpr $ shrink cnf
 
