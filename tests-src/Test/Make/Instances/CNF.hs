{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : Test.Make.Instances.CNF
Description : Tests the CNF Instances for Make
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Make CNF modules
-}

module Test.Make.Instances.CNF (
  tests                 ,-- :: TestTree
  appropriateCNFToConfig,-- :: CNF -> Maybe BoolSolution -> CNFConfig -> Property
  ) where

import qualified Data.Vector                         as V
import           HSat.Make.Instances.CNF
import           HSat.Problem.Instances.CNF.Internal
import           HSat.Problem.Instances.Common
import           HSat.Solution.Instances.CNF
import           TestUtils
import qualified Test.Make.Instances.CNF.Internal     as Internal (tests)
import           Test.Make.Instances.CNF.Internal     hiding (tests)

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    testGroup "makeCNF" [
       makeCNFTest1
       ],
    Internal.tests
    ]

makeCNFTest1 :: TestTree
makeCNFTest1 =
  testProperty "makeCNF returns valid CNF appropriate to config" $ monadicIO $ do
    config    <- pick arbitrary
    (cnf,sol) <- run $ makeCNF config
    return $ appropriateCNFToConfig cnf sol config

appropriateCNFToConfig                                   :: CNF -> Maybe BoolSolution -> CNFConfig -> Property
appropriateCNFToConfig cnf@CNF{..} boolSol CNFConfig{..} =
  checkBounds getClauseNumb getClauseSizeBounds .&&.
  propList (`checkBounds` getClauseSizeBounds) (getClauseSizes cnf) .&&.
  if not getVarsCanAppearTwice then
    checkDuplicateVariables cnf else
    property True .&&.
    checkBoundsVariables getMaxVar getClauseNumb getVariableBounds .&&.
    if getDefinitelyHasSolution then
      case boolSol of
       Nothing  -> property True
       Just sol -> property $ checkCNFSolution cnf sol else
      property True

checkDuplicateVariables         :: CNF -> Property
checkDuplicateVariables CNF{..} =
  listProperty (
    \clause ->
    counterexample ("Clause " ++ show clause ++ " contains duplicate variables")
    (not $ clauseContainsUniqueVars clause)
    ) $  V.toList . getVectClause $ getClauses
