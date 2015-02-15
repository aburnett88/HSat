

module Test.Make (
  tests
  ) where

import TestUtils
import HSat.Make
import HSat.Problem.ProblemExpr
import HSat.Problem
import qualified Test.Make.Internal as Internal
import qualified Test.Make.CNF as CNF
import qualified Test.Make.Config as Config

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
    Internal.tests,
    CNF.tests
    ]

makeTest1 :: TestTree
makeTest1 =
  testProperty "make configuration is valid" $ property (
    \bool -> bool == (not bool)
             )

  {-ioProperty (
    \config -> do
      result <- make config
      return . property . validate . getProblemExpr $ result
      )
-}
makeListTest1 :: TestTree
makeListTest1 =
  testProperty "make configurations are valid" $ property (
    \bool -> bool == (not bool)
             )
