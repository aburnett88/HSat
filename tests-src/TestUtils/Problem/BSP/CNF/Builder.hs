module TestUtils.Problem.BSP.CNF.Builder (
  genCNFBuilder,
  genCNFBuilderFinalise,
  genCNFBuilderEmptyClause,
  genCNFBuilderLitInClause,
  genCNFBuilderError,
  genCNFInvalidBuilder
  ) where

import TestUtils.Test
import HSat.Problem.BSP.CNF.Builder.Internal
import TestUtils.Problem.BSP.Common.Clauses
import TestUtils.Problem.BSP.Common.Clause
import Control.Monad (liftM5)
import Data.Word
import HSat.Problem.BSP.Common
import qualified Data.Vector as V
import HSat.Validate

genCNFBuilder :: Word -> Word -> Word -> Word -> Gen CNFBuilder
genCNFBuilder maxV clauseSize clausesSize maxVarOffset =
  oneof [
    genCNFBuilderFinalise maxV clauseSize clausesSize maxVarOffset,
    genCNFBuilderEmptyClause maxV clauseSize clausesSize maxVarOffset,
    genCNFBuilderLitInClause maxV clauseSize clausesSize maxVarOffset
    ]

instance Arbitrary CNFBuilder where
  arbitrary = genCNFBuilder 10 10 10 10
  shrink (CNFBuilder
          maxVar
          setClNumb
          currClNumb
          currClauses
          currClause) =
    map (\vect ->
          let currClNumb' = getSizeClauses vect
          in CNFBuilder maxVar setClNumb currClNumb' vect currClause) $
    shrink $ currClauses
    
instance Arbitrary CNFBuilderError where
  arbitrary = genCNFBuilderError
  shrink (IncorrectClauseNumber gotten expected) =
    filter validate . map (uncurry IncorrectClauseNumber) $ shrink (gotten,expected)



genCNFBuilderFinalise :: Word -> Word -> Word -> Word -> Gen CNFBuilder
genCNFBuilderFinalise maxV clauseSize clausesSize maxVarOffset = do
  clauses <-genClauses maxV clauseSize clausesSize
  let clNumb = getSizeClauses clauses
      maxVar = findMaxVar clauses
  maxVar' <- choose (maxVar,maxVarOffset+maxVar)
  return $ CNFBuilder maxVar' clNumb clNumb clauses emptyClause

genCNFBuilderEmptyClause :: Word -> Word -> Word -> Word -> Gen CNFBuilder
genCNFBuilderEmptyClause maxV clauseSize clausesSize maxVarOffset = do
  clauses <- genClauses maxV clauseSize clausesSize
  targetSize <- choose (getSizeClauses clauses +1, clausesSize)
  let maxVar = findMaxVar clauses
  maxVar' <- choose (maxVar,maxVarOffset + maxVar)
  return $ CNFBuilder maxVar' targetSize (getSizeClauses clauses) clauses emptyClause

genCNFBuilderLitInClause :: Word -> Word -> Word -> Word -> Gen CNFBuilder
genCNFBuilderLitInClause maxV clauseSize clausesSize maxVarOffset = do
  clauses <- genClauses maxV clauseSize clausesSize
  targetSize <- choose (getSizeClauses clauses + 1, clausesSize)
  let maxVar = findMaxVar clauses
  maxVar' <- choose (maxVar, maxVarOffset + maxVar)
  clause <- genClause maxV clauseSize
  let actualSize = if clauseIsEmpty clause then
                     (getSizeClauses clauses) else
                     (getSizeClauses clauses) + 1
  return $ CNFBuilder maxVar' targetSize actualSize clauses clause

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

genCNFInvalidBuilder :: Word -> Word -> Word -> Word -> Gen CNFBuilder
genCNFInvalidBuilder = genCNFBuilder
