module Test.Make.Config (
  tests
  ) where

import TestUtils
import HSat.Make.Config
import Control.Applicative
import System.Random
import Test.Problem.ProblemType ()
import HSat.Make.Internal

name :: String
name = "Config"

tests :: TestTree
tests =
  testGroup name []

instance Arbitrary Config where
  arbitrary = do
    out <- arbitrary
    input <- arbitrary
    return (Config out input)
  shrink (Config out input) =
    let xs = shrink (out,input)
    in map (uncurry Config) xs

instance Arbitrary ConfigProblemType where
  arbitrary = do
    oneof [
      CNFProblemType `liftA` arbitrary
      ]
  shrink (CNFProblemType cnf) =
    map CNFProblemType $ shrink cnf

instance Arbitrary CNFConfig where
  arbitrary = do
    clauses <- genBounded 0 (toEnum testMaxClausesSize)
    varsInEach <- genBounded 0 (toEnum testMaxClauseSize)
    vars <- do
      oneof [
        (Left) `liftA` genBounded (mkPosDouble 0.0) (mkPosDouble 5.0),
        (Right) `liftA` genBounded 0 2000
        ]
    posAndNeg <- arbitrary
    x <- arbitrary
    return $ CNFConfig clauses vars varsInEach posAndNeg x
  shrink (CNFConfig _ _ _ _ _) = []
--    let xs = shrink (a,b,c,d)
 --   in map (\(a,b,c,d) -> CNFConfig a b c d) xs

genBounded :: (Random a, Ord a, Bounded a) => a -> a -> Gen (Bounds a)
genBounded a b = return (mkBounds a b)
