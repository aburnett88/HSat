{-|
Module      : Test.Make.Config.Class
Description : Tests the Make Class file
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Class modules
-}

module Test.Make.Config.Class (
  tests -- TestTree
  ) where

import HSat.Make.Config.Class
import Test.Make.Instances.CNF.Internal (genCNFConfig)
import HSat.Make.Instances.CNF          ()
import TestUtils

name :: String
name = "Class"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkConfig" [
       mkConfigTest1
       ]
    ]

mkConfigTest1 :: TestTree
mkConfigTest1 =
  testProperty ("mkConfig a " `equiv` " Config a Nothing") $
  forAll
  (sized genCNFConfig)
  (\cnfConfig ->
    Config cnfConfig Nothing === mkConfig cnfConfig
  )

instance Arbitrary Config where
  arbitrary = oneof [
    mkConfig <$> sized genCNFConfig
    ]
  shrink _ = []
