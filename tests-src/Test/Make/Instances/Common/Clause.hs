{-|
Module      : Test.Make.Instances.Common.Clause
Description : Tests the Clause instances for Make
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Make Clause modules
-}

module Test.Make.Instances.Common.Clause (
  tests        ,-- :: TestTree
  ) where

import Control.Monad.Catch
import Control.Monad.State
import HSat.Make.Config.Class
import HSat.Make.Instances.Common.Clause
import HSat.Make.Instances.Common.Literal
import HSat.Problem.Instances.Common.Clause
import HSat.Solution.Instances.CNF
import TestUtils

name :: String
name = "Clause"

tests :: TestTree
tests =
  testGroup name [
    testGroup "makeClause" [
       makeClauseTest1,
       makeClauseTest2,
       makeClauseTest3,
       makeClauseTest4
       ]
    ]

genWords       :: Bool -> Int -> Gen (Word,Word)
genWords dup i = do
  let i' = toEnum i
  size <- choose (0,i')
  vars <- choose $ if dup then (1,i') else (size,size+i')
  return (size,vars)

genMakeClauseTest                                    :: String -> Bool -> Bool -> (((Bool,Clause),LiteralSet) -> Property) -> TestTree
genMakeClauseTest title duplicates solvable testFunc =
  testProperty title $ monadicIO $ do
    (size,vars) <- pick (sized $ genWords duplicates)
    literalSet  <- run $ mkLiteralSet vars duplicates
    stop =<< run (
      catch (
         do
           (result,_) <- runStateT (makeClause solvable size) literalSet
           return $ testFunc (result,literalSet)
         )
      (\exception ->
        let _ = exception :: MakeException
        in return $ counterexample "Unknown exception thrown" False
      )
      )

makeClauseTest1 :: TestTree
makeClauseTest1 =
  genMakeClauseTest "makeClause that is solvable and cannot have duplicates" False True
  (\((solvable,clause),literalSet) ->
    solvable                                           .&&.
    varsEvalToTrue clause (getTrueSet literalSet) >= 1 .&&.
    clauseContainsUniqueVars clause
  )
    
makeClauseTest2 :: TestTree
makeClauseTest2 =
  genMakeClauseTest "makeClause that might not be solvable and cannot have duplicates" False False
  (\((solvable,clause),literalSet) ->
    property $ (
      not solvable || (varsEvalToTrue clause (getTrueSet literalSet) >= 1)
      ) .&&.
    clauseContainsUniqueVars clause
    )

makeClauseTest3 :: TestTree
makeClauseTest3 =
  genMakeClauseTest "makeClause that might not be solvable and can have duplicates" False False
  (\((solvable,clause),literalSet) ->
    property (
      not solvable || (varsEvalToTrue clause (getTrueSet literalSet) >= 1)
      )                                      
  )

makeClauseTest4 :: TestTree
makeClauseTest4 =
  genMakeClauseTest "makeClause that is solvable and can have duplicates" True True
  (\((solvable,clause),literalSet) ->
        solvable                                           .&&.
        varsEvalToTrue clause (getTrueSet literalSet) >= 1
  )
