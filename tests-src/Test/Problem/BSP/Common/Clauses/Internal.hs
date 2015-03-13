{-|
Module      : Test.Problen.BSP.Common.Clauses.Internal
Description : The Clauses internal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the internal Clauses module
-}

module Test.Problem.BSP.Common.Clauses.Internal (
  tests,
  genClauses
  ) where

import HSat.Problem.BSP.Common.Clauses
import HSat.Problem.BSP.Common.Clauses.Internal
import TestUtils
import TestUtils.Validate
import qualified Data.Vector as V
import Control.Applicative
import Test.Problem.BSP.Common.Clause (genClause)
import Data.Word

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

{-
Non valid Clauses are when the size is incorrect when comparing to the size of
the vector containing the Literals themselves
-}
clausesTest2 :: TestTree
clausesTest2 =
  testProperty ("validate (Clauses vect (sizeVect + choose (1,maxBound))"++
                "== False") $
  forAll
  (do
      clauseList <- V.fromList `liftA` arbitrary
      wrongLen   <- choose (1,maxBound)
      return (clauseList,wrongLen)
  )
  (\(clauseList,wrongLen) ->
    let val = Clauses clauseList wrongLen
    in  not $ validate val
  )

{-
Checks that the length of the Clauses is consistent with the length of the vector containing the Clause, then checks each element to confirm that it is valid
-}
instance Validate Clauses where
  validate (Clauses clauseVector sizeClauses) =
    let actualSize = toEnum $ V.length clauseVector
    in (actualSize == sizeClauses) &&
       V.all validate clauseVector

genClauses :: Word -> Int -> Gen Clauses
genClauses maxVar size = do
  sizeOf <- choose (0,size)
  clauseVector <- V.replicateM (fromEnum sizeOf) $ genClause maxVar size
  return $ mkClauses clauseVector

instance Arbitrary Clauses where
  arbitrary      = sized $ genClauses maxBound
  shrink clauses =
    map mkClauses $ shrink . getVectClause $ clauses
