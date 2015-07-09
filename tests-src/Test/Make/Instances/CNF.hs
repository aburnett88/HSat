module Test.Make.Instances.CNF (
  tests       , -- TestTree
  genCNFConfig, -- Int -> CNFConfig
  ) where

import TestUtils
import HSat.Make.Instances.CNF
import HSat.Make.Common

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name []

instance Arbitrary CNFConfig where
  arbitrary = sized genCNFConfig
  shrink (CNFConfig {}) = []
--    let xs = shrink (a,b,c,d)
 --   in map (\(a,b,c,d) -> CNFConfig a b c d) xs

genCNFConfig :: Int -> Gen CNFConfig
genCNFConfig _ = do
  clauses <- genBounded 0 (toEnum testMaxClausesSize)
  varsInEach <- genBounded 0 (toEnum testMaxClauseSize)
  vars <-
    oneof [
      Left <$> genBounded (mkPosDouble 0.0) (mkPosDouble 5.0),
      Right <$> genBounded 0 2000
      ]
  posAndNeg <- arbitrary
  x <- arbitrary
  return $ CNFConfig clauses vars varsInEach posAndNeg x

genBounded :: (Bounded a, Ord a) => a -> a -> Gen (Bounds a)
genBounded l r = return $ mkBounds l r
