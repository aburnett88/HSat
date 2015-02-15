module TestUtils.Problem.ProblemType (
  ) where

import TestUtils.Test
import HSat.Problem.ProblemType

instance Arbitrary ProblemType where
  arbitrary = do
    typeOf <- choose (0,0) :: Gen Int
    return $ case typeOf of
      0 -> CNF
  shrink CNF = []
