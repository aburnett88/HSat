{-|
Module      : Test.Problen.BSP.Common.Clause.Internal
Description : The 'Clause' internal Test Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Clause definition
-}

module Test.Problem.BSP.Common.Clause.Internal (
  tests
  ) where

import TestUtils
import HSat.Problem.BSP.Common.Clause.Internal
import HSat.Problem.BSP.Common.Clause

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
  testProperty "validate arbitrary == True" $ property
  (\clause -> validate clause)

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
    let madeManual   = Clause litList wrongLen
    in  not $ validate madeManual
  )
