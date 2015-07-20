

module Test.Make (
  tests
  ) where

import TestUtils
import HSat.Make
import HSat.Problem.Internal
import qualified Test.Make.Config as Config
import qualified Test.Make.Common as Common
import qualified Test.Make.Instances as Instances

name :: String
name = "Make"

tests :: TestTree
tests =
  testGroup name [
    testGroup "make" [
       makeTest1
       ],
    testGroup "makeList" [
      makeListTest1
      ],
    testGroup "makeRetries" [],
    Config.tests,
    Common.tests,
    Instances.tests
    ]

genNoErrorsConfig :: Int -> Gen Config
genNoErrorsConfig _ = error "genNoErrosConfig not written yet"

makeTest1 :: TestTree
makeTest1 =
  testProperty "make configuration is valid" $ monadicIO $ do
    config <- pick (sized genNoErrorsConfig)
    problem <- run $ make config
    return $ meetsConfigCriteria problem config

meetsConfigCriteria :: Problem -> Config -> Property
meetsConfigCriteria _ _ = counterexample "Not written yet" False
    
makeListTest1 :: TestTree
makeListTest1 =
  testProperty "make configurations are valid" $ monadicIO $ do
    config <- pick (sized genNoErrorsConfig)
    size <- pick arbitrary
    retries <- return $ 20
    problems <- run $ makeList size retries config
    return $ testProblemsAndConfig problems config
    where
      testProblemsAndConfig :: [Problem] -> Config -> Property
      testProblemsAndConfig [] _ = property True
      testProblemsAndConfig (x:xs) c =
        meetsConfigCriteria x c .&&.
        testProblemsAndConfig xs c
