{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : Test.Make
Description : Tests the Make module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Make modules
-}

module Test.Make (
  tests, -- :: TestTree
  ) where

import           Data.Typeable                    (cast)
import           HSat.Make
import           HSat.Make.Config.Class           (mkConfig)
import           HSat.Problem.Internal
import           HSat.Problem.ProblemExpr.Class 
import qualified Test.Make.Common                 as Common
import qualified Test.Make.Config                 as Config
import qualified Test.Make.Instances              as Instances
import           Test.Make.Instances.CNF.Internal (genCNFConfig)
import           Test.Make.Instances.CNF          (appropriateCNFToConfig)
import           TestUtils

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
    Config.tests,
    Common.tests,
    Instances.tests
    ]

genNoErrorsConfig      :: Int -> Gen Config
genNoErrorsConfig size = oneof [
  mkConfig <$> genCNFConfig size
  ]

makeTest1 :: TestTree
makeTest1 =
  testProperty "make configuration is valid" $ monadicIO $ do
    config <- pick (sized genNoErrorsConfig)
    problem <- run $ make config
    return $ meetsConfigCriteria problem config

meetsConfigCriteria                    :: Problem -> Config -> Property
meetsConfigCriteria problem Config{..} =
  let p = problemExpr problem
  in meetsConfigCriteria' p
  where
    meetsConfigCriteria' ProblemExpr{..} =
      case (cast configuration,cast expr) of
       (Just cnf, Just cnfConfig) -> appropriateCNFToConfig cnf Nothing cnfConfig
       _                          -> counterexample "Unknown problem and config" False
    
makeListTest1 :: TestTree
makeListTest1 =
  testProperty "make configurations are valid" $ monadicIO $ do
    config      <- pick (sized genNoErrorsConfig)
    size        <- pick arbitrary
    let retries = 20
    problems    <- run $ makeList size retries config
    return $ testProblemsAndConfig problems config
    where
      testProblemsAndConfig          :: [Problem] -> Config -> Property
      testProblemsAndConfig [] _     = property True
      testProblemsAndConfig (x:xs) c =
        meetsConfigCriteria x c .&&.
        testProblemsAndConfig xs c
