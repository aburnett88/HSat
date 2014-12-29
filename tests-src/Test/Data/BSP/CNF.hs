{-|
Module      : Test.Data.BSP.CNF
Description : The CNF tests
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The tests for the CNF type
-}

module Test.Data.BSP.CNF (
  tests
  ) where

import TestUtils
import HSat.Data.BSP.CNF (CNF)
import qualified HSat.Data.BSP.CNF as CNF
import qualified Test.Data.BSP.CNF.Builder as Builder
import qualified Test.Data.BSP.CNF.Parser as Parser

name :: String
name = "Test.Data.BSP.CNF"

tests :: TestTree
tests =
  testGroup name [
    Builder.tests,
    Parser.tests,
    testProperty "fromClauses . toClauses == id" test1
    ]

test1     :: CNF -> Property
test1 cnf = property $ cnf == (
  CNF.fromClauses . CNF.toClauses $ cnf)
