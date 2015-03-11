module TestUtils.Problem.BSP.CNF.Builder (
  genCNFBuilder,
  genCNFBuilderFinalise,
  genCNFBuilderEmptyClause,
  genCNFBuilderLitInClause,
  genCNFBuilderError
 -- genCNFInvalidBuilder
  ) where

import TestUtils.Test
import HSat.Problem.BSP.CNF.Builder.Internal
import TestUtils.Problem.BSP.Common.Clauses
import TestUtils.Problem.BSP.Common.Clause
import Control.Monad (liftM5)
import Data.Word
import HSat.Problem.BSP.Common
import qualified Data.Vector as V
import TestUtils.Validate
import Debug.Trace

genCNFBuilder :: Int -> Gen CNFBuilder
genCNFBuilder size =
  oneof [
    genCNFBuilderFinalise size,
    genCNFBuilderEmptyClause size,
    genCNFBuilderLitInClause size
    ]

instance Arbitrary CNFBuilder where
  arbitrary = sized genCNFBuilder
  shrink (CNFBuilder
          maxVar
          setClNumb
          currClNumb
          currClauses
          currClause) =
    let f = (\(vect,clause) ->
          let size = sizeFunc vect clause
          in CNFBuilder maxVar setClNumb size vect clause)
    in map f $ shrink (currClauses,currClause)

sizeFunc :: Clauses -> Clause -> Word
sizeFunc cl c = (+) (getSizeClauses cl) $ 
                if clauseIsEmpty c then 0 else 1
    
instance Arbitrary CNFBuilderError where
  arbitrary = genCNFBuilderError
  shrink (IncorrectClauseNumber gotten expected) =
    filter validate . map (uncurry IncorrectClauseNumber) $
    shrink (gotten,expected)
  shrink (VarOutsideRange gotten expected) =
    filter validate . map (uncurry VarOutsideRange) $ shrink (gotten,expected)

genCNFBuilderFinalise :: Int -> Gen CNFBuilder
genCNFBuilderFinalise sized = do
  maxV <- choose (1,sized)
  clauses <- genClauses (toEnum maxV) sized
  let clNumb = getSizeClauses clauses
      maxVar = findMaxVar clauses
  return $ CNFBuilder maxVar clNumb clNumb clauses emptyClause

combined :: Int -> Gen (Clauses,Word,Word,Word)
combined sized = undefined
                 {-do
  clauses <- genClauses maxV clauseSize clausesSize
  targetSize <- choose (getSizeClauses clauses + 1, clausesSize + 1)
  let maxVar = findMaxVar clauses
  maxVar' <- choose (maxVar, maxVarOffset + maxVar)
  return (clauses,targetSize,maxVar,maxVar')-}

genCNFBuilderEmptyClause :: Int -> Gen CNFBuilder
genCNFBuilderEmptyClause sized = do
  (clauses,targetSize,maxVar,maxVar') <- combined sized
  return $
    CNFBuilder maxVar' targetSize (getSizeClauses clauses) clauses emptyClause

genCNFBuilderLitInClause :: Int -> Gen CNFBuilder
genCNFBuilderLitInClause sized = do
  (clauses,targetSize,maxVarCurr,maxVarAllowed) <- combined sized
  clause <- genClause maxVarCurr sized
  let actualSize = sizeFunc clauses clause
      targetSize' = (+) targetSize (if clauseIsEmpty clause then 0 else 1)
      builder     = CNFBuilder maxVarAllowed targetSize' actualSize clauses clause
  return builder

genCNFBuilderError :: Gen CNFBuilderError
genCNFBuilderError =
  oneof [
    do
      expected <- choose (0,maxBound)
      gotten   <- choose (1,maxBound)
      let val  = IncorrectClauseNumber (expected+gotten) expected
      return val
    ,
    do
      expected <- choose (0,maxBound - 1)
      gotten   <- choose (expected + 1, maxBound)
      let val  = VarOutsideRange (toInteger gotten) expected
      return val
    ]

{-
genCNFInvalidBuilder :: Word -> Word -> Word -> Word -> Gen CNFBuilder
genCNFInvalidBuilder =
  genCNFInvalidBuilderCount

genCNFInvalidBuilderCount :: Word -> Word -> Word -> Word -> Gen CNFBuilder
genCNFInvalidBuilderCount var sizeClause sizeClauses offset = do
  builder <- genCNFBuilder var sizeClause sizeClauses offset
  x <- choose (1,maxBound)
  return $ builder {
    getCurrClNumb = getCurrClNumb builder + x
    }
-}
instance Validate CNFBuilder where
  validate (CNFBuilder
            exptdMaxVar
            exptdClNumb
            currClNumb
            currClauses
            currClause) =
    let computedSize = sizeFunc currClauses currClause
    in (exptdClNumb >= currClNumb)                      &&
       V.all testVarInRange (getVectClause currClauses) &&
       (computedSize == currClNumb)                     &&
       testVarInRange currClause                        &&
       validate currClauses                             &&
       validate currClause
    where
      testVarInRange :: Clause -> Bool
      testVarInRange cl = V.all (varInRange exptdMaxVar) .
                          V.map getVariable $ getVectLiteral cl

instance Validate CNFBuilderError where
  validate (IncorrectClauseNumber gotten expected) =
    expected /= gotten
  validate (VarOutsideRange gotten expected)       =
    ((toInteger expected) < gotten) ||
    (gotten == 0)
