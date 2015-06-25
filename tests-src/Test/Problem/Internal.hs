{-# LANGUAGE RecordWildCards #-}

module Test.Problem.Internal (
  tests
  ) where

import TestUtils
import Control.Applicative
import HSat.Problem.Internal
import Test.Problem.Source ()
import Test.Problem.ProblemExpr.Class ()

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name []


instance Arbitrary Problem where
  arbitrary = liftA2 MkProblem arbitrary arbitrary
  shrink MkProblem{..} =
    map (uncurry MkProblem) $ shrink (source,problemExpr)
