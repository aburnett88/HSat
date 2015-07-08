{-|
Module      : Test.Problen.Instances.CNF.Internal
Description : The tests for the Internal CNF module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Node for the Internal CNF module
-}

module Test.Problem.Instances.CNF.Internal (
  tests
  ) where

import TestUtils
import HSat.Problem.Instances.CNF
import TestUtils.Validate


name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    testCNF
    ]

testCNF :: TestTree
testCNF =
  testProperty "Arbitrary CNF are valid" $ property testCNF'
  where
    testCNF' :: CNF -> Bool
    testCNF' = validate 

instance Arbitrary CNF where
  arbitrary = undefined

instance Validate CNF where
  validate = undefined
