{-# LANGUAGE
    RecordWildCards
    #-}

module Test.Make.Instances.CNF (
  tests       , -- TestTree
  genCNFConfig, -- Int -> CNFConfig
  ) where

import TestUtils
import HSat.Make.Instances.CNF
import HSat.Make.Common
import HSat.Problem.Instances.CNF.Internal
import HSat.Solution.Instances.CNF
import Data.Maybe (fromJust)

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    testGroup "makeCNF" [
       makeCNFTest1
       ]
    ]

makeCNFTest1 :: TestTree
makeCNFTest1 =
  testProperty "makeCNF returns valid CNF appropriate to config" $ monadicIO $ do
    config <- pick arbitrary
    (cnf,sol) <- run $ makeCNF config
    return $ appropriateCNFToConfig cnf sol config

appropriateCNFToConfig :: CNF -> Maybe BoolSolution -> CNFConfig -> Property
appropriateCNFToConfig cnf@CNF{..} boolSol CNFConfig{..} =
  let _ = 2
  in checkBounds getClauseNumb getClauseSizeBounds .&&.
     propList (flip checkBounds getClauseSizeBounds) (getClauseSizes cnf) .&&.
     if (not getVarsCanAppearTwice) then
       checkDuplicateVariables cnf else
       property True .&&.
       checkBounds getMaxVar (case getVariableBounds of
                               Right set -> set
                               Left double -> _ getClauseNumb double) .&&.
       if getDefinitelyHasSolution then
         property $ checkCNFSolution cnf (fromJust boolSol) else
         property True

checkDuplicateVariables :: CNF -> Property
checkDuplicateVariables = _

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
