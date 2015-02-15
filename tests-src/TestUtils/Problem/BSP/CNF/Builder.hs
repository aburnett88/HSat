module TestUtils.Problem.BSP.CNF.Builder (
  genCNFBuilderNonFinal,
  genCNFBuilderFinalClause,
  genBadCNFBuilderError,
  genCNFBuilderError,
  genCNFBuilderBad
  ) where

import TestUtils.Test
import HSat.Problem.BSP.CNF.Builder.Internal
import TestUtils.Problem.BSP.Common.Clauses
import TestUtils.Problem.BSP.Common.Clause

instance Arbitrary CNFBuilder where
  arbitrary = oneof [genCNFBuilderNonFinal,genCNFBuilderFinalClause]
  shrink _ = []

instance Arbitrary CNFBuilderError where
  arbitrary = genCNFBuilderError
  shrink _ = []

genCNFBuilderNonFinal :: Gen CNFBuilder
genCNFBuilderNonFinal = do
  targetSize <- choose (1,100)
  currentSize <- choose (0,targetSize - 1)
  maxVar <- choose (1,100)
  clauses <- arbitrary
  clause <- arbitrary
  return $ CNFBuilder maxVar targetSize currentSize clauses clause

genCNFBuilderFinalClause :: Gen CNFBuilder
genCNFBuilderFinalClause = do
  currentSize <- choose (0,100)
  maxVar <- choose (0,100)
  clauses <- arbitrary
  clause <- arbitrary
  return $ CNFBuilder maxVar currentSize currentSize clauses clause

genBadCNFBuilderError :: Gen CNFBuilderError
genBadCNFBuilderError = do
  typeOf <- choose (0,1) :: Gen Int
  case typeOf of
    0 -> do
      expected <- choose (0,100)
      gotten <- choose (0,expected -1)
      return $ IncorrectClauseNumber gotten expected
    1 -> do
      expected <- choose (0,100)
      gotten <- choose (0,expected - 1)
      return $ LitOutsideRange gotten expected

genCNFBuilderError :: Gen CNFBuilderError
genCNFBuilderError = do
  typeOf <- choose (0,1) :: Gen Int
  case typeOf of
    0 -> do
      expected <- choose (0,100)
      gotten <- choose (0,expected)
      return $ IncorrectClauseNumber gotten expected
    1 -> do
      expected <- choose (0,100)
      gotten <- choose (0,100)
      return $ LitOutsideRange gotten expected

genCNFBuilderBad :: Gen CNFBuilder
genCNFBuilderBad = arbitrary
