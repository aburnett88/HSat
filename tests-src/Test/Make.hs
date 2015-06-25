

module Test.Make (
  tests
  ) where

import TestUtils
import HSat.Make
import HSat.Problem.Internal
import qualified Test.Make.Config as Config
import qualified Test.Make.Common as Common
import qualified Test.Make.Instances as Instances
import Control.Monad.Catch

name :: String
name = "Make"

tests :: TestTree
tests =
  testGroup name [
    testGroup "make" [
       makeTest1,
       makeTest2
       ],
    testGroup "makeList" [
      makeListTest1
      ],
    Config.tests,
    Common.tests,
    Instances.tests
    ]

makeTest1 :: TestTree
makeTest1 =
  testProperty "make configuration is valid" $ monadicIO $ do
    config <- pick arbitrary
    run $ catchAll (
      do
        problem <- make config True
        return $ testCorrectType config problem
        )
      (\exception -> return $ counterexample ("Exception thrown" ++ show exception) False)
                     

makeTest2 :: TestTree
makeTest2 =
  testProperty "make configuration is valid" $ monadicIO $ do
    config <- pick arbitrary
    run $ catchAll (
      do
        problem <- make config False
        return $ testCorrectType config problem 
        )
      (\exception -> return $ testError config exception)

testError :: Config -> SomeException -> Property
testError _ _ = undefined

testCorrectType :: Config -> Problem -> Property
testCorrectType _ _ = undefined
    
makeListTest1 :: TestTree
makeListTest1 =
  testProperty "make configurations are valid" $ ioProperty $ do
    config <- generate arbitrary
    size <- generate $ choose (0,20)
    problems <- makeList size config
    return $ testProblemsAndConfig problems config
    where
      testProblemsAndConfig :: [Problem] -> Config -> Property
      testProblemsAndConfig [] _ = property True
      testProblemsAndConfig (x:xs) c =
        testCorrectType c x .&&.
        testProblemsAndConfig xs c
