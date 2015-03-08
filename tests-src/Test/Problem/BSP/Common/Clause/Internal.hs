{-|
Module      : Test.Problen.BSP.Common.Clause.Internal
Description : The Clause internal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the internal Clause module
-}

module Test.Problem.BSP.Common.Clause.Internal (
  tests
  ) where

import HSat.Problem.BSP.Common.Clause
import HSat.Problem.BSP.Common.Clause.Internal
import TestUtils
import TestUtils.Validate

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    clauseTest1,
    clauseTest2
    ]

clauseTest1 :: TestTree
clauseTest1 =
  testProperty "validate arbitrary == True" $ property testClause
  where
    testClause :: Clause -> Bool
    testClause = validate

{-
Incorrect size should fail to validate
-}
clauseTest2 :: TestTree
clauseTest2 =
  testProperty ("validate (Clause vect (sizeVect + choose (1,maxBound))"++
                "== False") $
  forAll
  (do
      litList  <- arbitrary
      wrongLen <- choose (1,maxBound)
      return (litList,wrongLen)
  )
  (\(litList,wrongLen) ->
    let val = Clause litList wrongLen
    in  not $ validate val
  )
