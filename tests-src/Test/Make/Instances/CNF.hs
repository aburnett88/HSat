module Test.Make.Instances.CNF (
  tests
  ) where

import TestUtils
import HSat.Make.Instances.CNF

instance Arbitrary CNFConfig where
  arbitrary = do
    clauses <- genBounded 0 (toEnum testMaxClausesSize)
    varsInEach <- genBounded 0 (toEnum testMaxClauseSize)
    vars <-
      oneof [
        Left `liftA` genBounded (mkPosDouble 0.0) (mkPosDouble 5.0),
        Right `liftA` genBounded 0 2000
        ]
    posAndNeg <- arbitrary
    x <- arbitrary
    return $ CNFConfig clauses vars varsInEach posAndNeg x
  shrink (CNFConfig {}) = []
--    let xs = shrink (a,b,c,d)
 --   in map (\(a,b,c,d) -> CNFConfig a b c d) xs
