

module Test.Make (
  tests
  ) where

import TestUtils
import HSat.Make
import HSat.Problem.ProblemExpr
import HSat.Problem
import qualified Test.Make.BSP as BSP
import qualified Test.Make.Config as Config
import qualified Test.Make.Internal as Internal
import HSat.Make.Config

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
    BSP.tests,
    Internal.tests
    ]

makeTest1 :: TestTree
makeTest1 =
  testProperty "make configuration is valid" $ ioProperty $ do
    config <- generate arbitrary
    problem <- make config True
    return $ case problem of
      Left e -> counterexample
                ("Unexpected Left: " ++ show e) False
      Right p -> testProblemAndConfig1 config p

makeTest2 :: TestTree
makeTest2 =
  testProperty "make configuration is valid" $ ioProperty $ do
    config <- generate arbitrary
    problem <- make config False
    return $ case problem of
      Left e -> testError config e
      Right r -> testProblemAndConfig r config

  {-ioProperty (
      result <- make config
      return . property . validate . getProblemExpr $ result
      )
-}

testError :: Config -> MakeError -> Property
testError _ _ =
  counterexample "testError not written yet" False

testProblemAndConfig1 :: Config -> Problem -> Property
testProblemAndConfig1 _ _ =
  counterexample "testProblemAndConfig1 not written yet" False

testProblemAndConfig :: Problem -> Config -> Property
testProblemAndConfig _ _ =
  counterexample "testProblemAndConfig not written yet" False
    
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
        (testProblemAndConfig1 c x) .&&.
        (testProblemsAndConfig xs c)
