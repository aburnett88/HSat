module TestUtils.Test (
  module Test.Extended,
  module Control.Monad
  ) where

import Test.Tasty as Test.Extended
import Test.Tasty.Golden as Test.Extended
import Test.Tasty.HUnit as Test.Extended
import Test.Tasty.QuickCheck as Test.Extended
import Control.Monad (liftM,liftM2)
import qualified Data.Vector as V

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = do
    V.fromList `liftM` listOf arbitrary
  shrink v =
    map V.fromList $ shrink . V.toList $ v


