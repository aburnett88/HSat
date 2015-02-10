module Test.Make.CNF.Internal (
  tests
  ) where

import           Control.Monad (replicateM)
import           Control.Monad.Random.Class
import qualified Data.Set as S
import qualified Data.Vector as V
import           HSat.Make.CNF.Internal
import           HSat.Make.Config
import           HSat.Make.Internal
import           HSat.Problem.BSP.Common
import           TestUtils

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name []
  {-
    testGroup "fillClauses" [
       fillClausesTest1
       ],
    testGroup "chooseVariables" [
      chooseVariablesTest1
      ],
    testGroup "chooseClauses" [
       chooseClausesTest1
       ],
    testGroup "evalClauseSizes" [
       evalClauseSizesTest1
       ]
    ]

chooseVariablesTest1 :: TestTree
chooseVariablesTest1 =
  testProperty "chooseVariables satisfies invariants" $ ioProperty $ do
    vpc <- generate arbitrary
    res <- chooseVariables vpc
    return $ property $ (
      checkBounds res vpc
        )
    

fillClausesTest1 :: TestTree
fillClausesTest1 =
  testProperty "fillClauses satisfies invariants" $ ioProperty $ do
    clausesSize <- generate $ choose (5,500)
    clauseSizes <- generate $ do
      replicateM (fromEnum clausesSize) $ choose (2,10)
    let totalVars = sum clauseSizes
    trueOrFalse <- generate arbitrary
    --only generate the variables that can actually produce things that work - for now
    vars <- generate $ choose (1,if trueOrFalse then
                                   totalVars `div` 2 else
                                   totalVars)
    clauses' <- fillClauses clausesSize clauseSizes totalVars trueOrFalse vars
    case clauses' of
      (Left r) -> return . property $ False
      (Right clauses) -> do
        --setting up everything to test
        let
          setOfVars = S.fromList . map mkVariable $ [1..vars]
          actualLen = V.length . getVectOfClauses $ clauses
          expectedLen = fromEnum clausesSize
          actualSizes = (V.toList . V.map clauseLength . getVectOfClauses $ clauses)
          varsNotInClauses = S.difference setOfVars $ getSetOfVars clauses
          posVarsNotInClauses = S.difference setOfVars $ getSetPos clauses
          negVarsNotInClauses = S.difference setOfVars $ getSetNeg clauses
        return . property $ (
          (testEq "Length of Clauses incorrect" actualLen expectedLen) .&&.
          (testAllEq "Length of individual clause incorrect" actualSizes clauseSizes) .&&.
          (if trueOrFalse then
             (testEq "Positive Variables in Clauses incorrect" posVarsNotInClauses S.empty) .&&.
             (testEq "Negative Variables in Clauses incorrect" negVarsNotInClauses S.empty) else
             (testEq "Variables in Clauses incorrect" varsNotInClauses S.empty))
          )


      
chooseClausesTest1 :: TestTree
chooseClausesTest1 =
  testProperty "ChooseClauses non-relaion returns correct values" $ ioProperty $ do
    c <- generate arbitrary
    c' <- chooseClauses c
    return $ property $ (checkBounds c' c)

evalClauseSizesTest1 :: TestTree
evalClauseSizesTest1 =
  testProperty "evalClauseSizes sizes correct" $ ioProperty $ do
    (w,vpc) <- generate $ do
      w' <- choose (0,fromIntegral testMaxClausesSize)
      vpc' <- arbitrary
      return (w',vpc')
    s <- generate $ arbitrary
    (vpec,xs,total) <- evalClauseSizes w vpc s
    return $ property $ (testList vpc xs) .&&. (sum xs==total)

-}
