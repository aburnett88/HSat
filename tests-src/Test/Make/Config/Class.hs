module Test.Make.Config.Class (
  tests
  ) where

import TestUtils
import HSat.Make.Config.Class
import Control.Applicative
import Test.Make.Instances.CNF (genCNFConfig)

name :: String
name = "Class"

tests :: TestTree
tests =
  testGroup name []

instance Arbitrary Config where
  arbitrary = oneof [
    liftA2 Config (sized genCNFConfig) (return Nothing)
    ]
  shrink _ = []
