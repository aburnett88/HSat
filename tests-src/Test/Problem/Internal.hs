{-# LANGUAGE RecordWildCards #-}

module Test.Problem.Internal (
  tests
  ) where

import TestUtils
import Control.Applicative
import HSat.Problem.Internal
import Test.Problem.Source ()
import Test.Problem.ProblemExpr ()

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name []


instance Arbitrary Problem where
  arbitrary = liftA2 Problem arbitrary arbitrary
  shrink Problem{..} =
    map (uncurry Problem) $ shrink (source,problemExpr)
