{-|
Module      : Test.Problem.ProblemExpr.Class
Description : The ProblemExpr Class tests
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Exports the testing functionality of the ProblemExpr Class
-}

module Test.Problem.ProblemExpr.Class (
  tests
  ) where

import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.CNF.Internal
import HSat.Problem.ProblemExpr.Class
import TestUtils

name :: String
name = "Class"

tests :: TestTree
tests =
  testGroup name [
    testGroup "fromProblemExpr" [
       testFromProblemExpr1
       ]
    ]

testFromProblemExpr1 :: TestTree
testFromProblemExpr1 =
  testProperty ("fromProblemExpr (ProblemExpr cnf) " `equiv` " cnf") $ forAll
  (sized generateCNF)
  (\cnf ->
    case fromProblemExpr $ ProblemExpr cnf of
      Just cnf'@CNF{} -> cnf === cnf'
      Nothing         -> counterexample "Incorrect value from fromProblemExpr" False
  )

instance Arbitrary ProblemExpr where
  arbitrary = oneof [
    ProblemExpr <$> sized generateCNF
    ]
  shrink problemExpr =
    case fromProblemExpr problemExpr of
     Just cnf@CNF{} -> map ProblemExpr . shrinkCNF $ cnf
     Nothing        -> []

generateCNF :: Int -> Gen CNF
generateCNF = undefined

shrinkCNF :: CNF -> [CNF]
shrinkCNF = undefined
