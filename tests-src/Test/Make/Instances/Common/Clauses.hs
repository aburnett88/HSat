{-|
Module      : Test.Make.Instances.Common.Clauses
Description : Tests the Clauses instances for Make
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Make Clauses modules
-}

module Test.Make.Instances.Common.Clauses (
  tests,-- :: TestTree
  ) where

import           Control.Monad.Catch
import           Control.Monad.State
import qualified Data.Vector                        as V
import           HSat.Make.Config.Class
import           HSat.Make.Instances.Common.Clauses
import           HSat.Make.Instances.Common.Literal
import           HSat.Problem.Instances.Common
import           HSat.Solution.Instances.CNF
import           TestUtils

name :: String
name = "Clauses"

tests :: TestTree
tests =
  testGroup name [
    testGroup "makeClauses" [
       makeClausesTest1,
       makeClausesTest2,
       makeClausesTest3,
       makeClausesTest4
       ]
    ]

genWordsList       :: Bool -> Int -> Gen ([Word],Word)
genWordsList dup i = do
  let i' = toEnum i
  sizes  <- listOf $ choose (0,i')
  let m  = if null sizes then 0 else maximum sizes
  vars   <- choose $ if dup then (1,i') else (m,m+i')
  return (sizes,vars)

makeClausesGenTest                                    :: String -> Bool -> Bool -> ((Clauses,LiteralSet) -> Property) -> TestTree
makeClausesGenTest title duplicates solvable testFunc =
  testProperty title $ monadicIO $ do
    (sizes,vars) <- pick (sized $ genWordsList duplicates)
    literalSet   <- run $ mkLiteralSet vars duplicates
    stop =<< run (
      catch (
         do
           result <- runStateT (makeClauses sizes solvable) literalSet
           return $ testFunc result
         )
      (\exception ->
        let _ = exception :: MakeException
        in return $ counterexample "Unknown exception thrown" False
      )
      )

makeClausesTest1 :: TestTree
makeClausesTest1 =
  makeClausesGenTest "makeClauses solvable and allow duplicates" True True
  (uncurry isSolvable)

isSolvable                    :: Clauses -> LiteralSet -> Property
isSolvable clauses literalSet =
  listProperty (\clause ->
                 property (varsEvalToTrue clause (getTrueSet literalSet) >= 1)
               ) (V.toList . getVectClause $ clauses)

noDuplicates         :: Clauses -> Property
noDuplicates clauses =
  listProperty (property . clauseContainsUniqueVars) (V.toList . getVectClause $ clauses)

makeClausesTest2 :: TestTree
makeClausesTest2 =
  makeClausesGenTest "makeClauses solvable and not allow duplicates" False True
  (\(clauses,literalSet) ->
    isSolvable clauses literalSet .&&.
    noDuplicates clauses
  )

makeClausesTest3 :: TestTree
makeClausesTest3 =
  makeClausesGenTest "makeClauses maybe not solvable and not allow duplicates" False False
  (\(clauses,_) ->
    noDuplicates clauses
  )

makeClausesTest4 :: TestTree
makeClausesTest4 =
  makeClausesGenTest "makeClauses maybe not solvable and allow duplicates" True False
  (\_ -> property True)
