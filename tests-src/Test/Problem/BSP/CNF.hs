{-|
Module      : Test.Problen.BSP.CNF
Description : The CNF TestTree Node
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Node for the CNF module
-}

module Test.Problem.BSP.CNF (
  tests
  ) where

import           HSat.Problem.BSP.CNF
import qualified Test.Problem.BSP.CNF.Builder as CNFBuilder
import qualified Test.Problem.BSP.CNF.Internal as Internal
import           TestUtils
import TestUtils.Validate

name :: String
name = "CNF"

tests :: TestTree
tests = testGroup name [
  Internal.tests,
  CNFBuilder.tests,
  testCNF,
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




