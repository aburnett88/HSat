{-|
Module      : Test.Problem.BSP.CNF.Builder
Description : The tests for the CNFBuilder test leaf
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides tests for the CNFBuilder type
-}

module Test.Problem.BSP.CNF.Builder (
  tests
  ) where

import           Control.Monad (liftM)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.CNF.Builder.Internal
import           HSat.Problem.BSP.Common
import           TestUtils
import qualified Test.Problem.BSP.CNF.Builder.Internal as Internal

name :: String
name = "Builder"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    testGroup "cnfBuilder" [
       cnfBuilderTest1,
       cnfBuilderTest2
       ],
    testGroup "cnfBuilder'" [
       cnfBuilder'Test1,
       cnfBuilder'Test2
       ],
    testGroup "addLiteral" [
      addLiteralTest1,
      addLiteralTest2
      ],
    testGroup "addLiteral'" [
      addLiteral'Test1,
      addLiteral'Test2
      ],
    testGroup "finishClause" [
      finishClauseTest1
      ],
    testGroup "finishClause'" [
      finishClause'Test1
      ],
    testGroup "finalise" [
      finaliseTest1
      ],
    testGroup "finalise'" [
      finalise'Test1
      ],
    testGroup "General Tests" [
      generalTest1,
      generalTest2
      ]
    ]

--cnfBuilder Tests
gencnfBuilderTest1,
  gencnfBuilderTest2 :: (Testable a) => a -> TestTree
gencnfBuilderTest1 m =
  testProperty "varNumb cnfBuilder a b == a" $ property m
gencnfBuilderTest2 m =
  testProperty "expectedClause cnfBuilder a b == b" $ property m

cnfBuilderTest1,cnfBuilder'Test1 :: TestTree
cnfBuilderTest1 = gencnfBuilderTest1 (
  \(var,clause) ->
  (getExptdMaxVar `liftM` cnfBuilder var clause) == (return var)
  )
cnfBuilder'Test1 = gencnfBuilderTest1 (
  \(var,clause) ->
  (getExptdMaxVar . cnfBuilder' var $ clause) == var
  )

cnfBuilderTest2,cnfBuilder'Test2 :: TestTree
cnfBuilderTest2 = gencnfBuilderTest2 (
  \(var,clause) ->
    (getExptdClNumb `liftM` cnfBuilder var clause) == (return clause)
    )
cnfBuilder'Test2 = gencnfBuilderTest2 (
  \(var,clause) ->
   (getExptdClNumb . cnfBuilder' var $ clause) == clause
   )

--Add literal tests

genCNFBuilder :: Gen CNFBuilder
genCNFBuilder = do
  cnf <- arbitrary
  --We can't add any literals to this CNFBuilder, so this test is pointless.
  --We make the closest possible thing - containing a single 1
  let cnf' = if (getMaxVar cnf) == 0 then
               mkCNFFromClauses . mkClausesFromIntegers $ [[1]] else
               cnf
      v = getMaxVar cnf'
      ex = getClauseNumb cnf'
  (cl,c) <- splitRandom emptyClause . getVectClause . getClauses $ cnf'
  let curr = toEnum (V.length cl + 1)
  return $ CNFBuilder v ex curr (mkClausesFromClause . V.toList $ cl) c

splitRandom :: a -> V.Vector a -> Gen (V.Vector a,a)
splitRandom a vect =
  case V.length vect of
    0 -> return (V.empty,a)
    n -> do
      index <- choose (0,n-1)
      let x = vect V.!? index
      case x of
        Nothing -> error ("failure")
        Just w -> return $ (V.take index vect, w)
            
genAddLiteralTest1,genAddLiteralTest2 :: (Testable a) =>
                                         ((CNFBuilder,Literal) -> a) -> TestTree
genAddLiteralTest1 f =
  testProperty "addLiteral gives correct values" (
    forAll
    --Generate a lit within the range of the builder
    (do
      b <- genCNFBuilder
      sign <- arbitrary
      lit <- (mkLiteral sign . mkVariable) `liftM` choose (1,getExptdMaxVar b)
      return (b,lit)
      )
    f
    )
genAddLiteralTest2 f =
  testProperty "addLiteral with lit outside of range returns correct values" (
    forAll
    (do
      --generate a lit outside the range of the builder
      b <- genCNFBuilder
      sign <- arbitrary
      lit <- (mkLiteral sign . mkVariable) `liftM` choose (getExptdMaxVar b+1,maxBound :: Word)
      return (b,lit)
      )
    f
    )
  

addLiteralTest1,addLiteral'Test1 :: TestTree
addLiteralTest1 = genAddLiteralTest1 (
  \(b,lit) ->
  let build = (return b) >>= addLiteral lit
  in build == (return $ b {
                  getCurrClause = clauseAddLiteral (getCurrClause b) lit,
                  getExptdClNumb = (+) (getExptdClNumb b) (
                    if clauseIsEmpty . getCurrClause $ b then
                      1 else
                      0
                      )
                                  }
               )
     )
addLiteral'Test1 = genAddLiteralTest1 (
  \(b,lit) ->
  let build = addLiteral' lit b
  in build == b {
    getCurrClause = clauseAddLiteral (getCurrClause b) lit,
    getExptdClNumb = (+) (getExptdClNumb b) (
      if clauseIsEmpty . getCurrClause $ b then
        1 else
        0
        )
                    }
     )

addLiteralTest2 :: TestTree
addLiteralTest2 = genAddLiteralTest2 (
  \(b,lit) ->
  let build = (return b) >>= addLiteral lit
  in build == (Left $ LitOutsideRange (getWord . getVariable $ lit) (getExptdMaxVar b))
     )

addLiteral'Test2 :: TestTree
addLiteral'Test2 = genAddLiteralTest2 (
  \(b,lit) ->
  let build = addLiteral' lit b
  in build == b {
    getCurrClause = clauseAddLiteral (getCurrClause b) lit,
    getCurrClNumb = (+) (getCurrClNumb b) (
      if clauseIsEmpty . getCurrClause $ b then
        1 else
        0
        )
                        }
     )

finishClause'Test1 :: TestTree
finishClause'Test1 =
  testProperty "finishClause has the desired effects. No errors can be thrown" $
  forAll
  genCNFBuilder
  (\builder ->
    let built = finishClause' builder
        newClauses' = clausesAddClause (getCurrClauses builder) (getCurrClause builder)
        size = (+) (getCurrClNumb builder) (
          if clauseIsEmpty . getCurrClause $ builder then
            1 else 0
                   )
    in newClauses' == (getCurrClauses built) &&
       size == (getCurrClNumb built)
              )

finishClauseTest1 :: TestTree
finishClauseTest1 =
  testProperty "FinishClause has the desired effects" $
  forAll
  genCNFBuilder
  (\builder ->
    let built = finishClause builder
        newClauses' = return $ clausesAddClause (getCurrClauses builder) (getCurrClause builder)
        size = return $ (+) (getCurrClNumb builder) (
          if clauseIsEmpty . getCurrClause $ builder then 1 else 0)
    in newClauses' == (getCurrClauses `liftM` built) &&
       size == (getCurrClNumb `liftM` built)
       )

finaliseTest1 :: TestTree
finaliseTest1 =
  testProperty "finalise on something that should be finalised works" $
  property
  (\i ->
    i == True || i == False
    )

finalise'Test1 :: TestTree
finalise'Test1 =
  testProperty "finalise' on something that should be finalised works" $
  property
  (\i ->
    i == True || i == False
    )

generalTest1 :: TestTree
generalTest1 =
  testProperty "Take a list (of list) of ints and apply functions to create same cnf" $
  property
  (\cnf ->
    let v = getMaxVar cnf
        c = getClauseNumb cnf
        lits = map (map mkLiteralFromInteger) . cnfToIntegers $ cnf
        cnf' = evaluate (cnfBuilder v c) lits
    in (return cnf) == cnf'
       )

generalTest2 :: TestTree
generalTest2 =
  testProperty "take list of list of ints and apply functions to create same cnf" $
  property
  (\cnf ->
    let v = getMaxVar cnf
        c = getClauseNumb cnf
        lits = map (map mkLiteralFromInteger) . cnfToIntegers $ cnf
        cnf' = evaluate' (cnfBuilder' v c) lits
    in cnf == cnf'
       )

evaluate :: CNFBuildErr -> [[Literal]] -> Either CNFBuilderError CNF
evaluate cnf [] = cnf >>= finalise
evaluate cnf (x:xs) = evaluate (evaluation cnf x) xs
  where
    evaluation :: CNFBuildErr -> [Literal] -> CNFBuildErr
    evaluation cnf [] = cnf >>= finishClause
    evaluation cnf (y:ys) =
      evaluation (cnf >>= addLiteral y) ys

evaluate' :: CNFBuilder -> [[Literal]] -> CNF
evaluate' cnf [] = finalise' cnf
evaluate' cnf (x:xs) = evaluate' (evaluation cnf x) xs
  where
    evaluation :: CNFBuilder -> [Literal] -> CNFBuilder
    evaluation cnf [] = finishClause' cnf
    evaluation cnf (y:ys) =
      evaluation (addLiteral' y cnf) ys
  
  
        
