{-|
Module      : Test.Problen.BSP.Common.Clauses.Internal
Description : The 'Clause' internal Test Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Clauses definition
-}

module Test.Problem.BSP.Common.Clauses.Internal (
  tests
  ) where

import HSat.Problem.BSP.Common.Clauses
import HSat.Problem.BSP.Common.Clauses.Internal
import HSat.Validate
import TestUtils

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    clausesTest1,
    clausesTest2
    ]

clausesTest1 :: TestTree
clausesTest1 =
  testProperty "validate arbitrary == True" $ property testClauses
  where
    testClauses :: Clauses -> Bool
    testClauses = validate

clausesTest2 :: TestTree
clausesTest2 =
  testProperty ("validate (Clauses vect (sizeVect + choose (1,maxBound))"++
                "== False") $
  forAll
  (do
      clauseList <- arbitrary
      wrongLen   <- choose (1,maxBound)
      return (clauseList,wrongLen)
      )
  (\(clauseList,wrongLen) ->
    let madeManual   = Clauses clauseList wrongLen
    in  not $ validate madeManual
  )
