{-|
Module      : Test.Data.BSP.Common.Clauses
Description : Tests for the Clauses data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown


-}
module Test.Data.BSP.Common.Clauses (
  tests
  ) where

import TestUtils
import qualified Data.Vector as V (length)
import HSat.Data.BSP.Common.Clauses (Clauses)
import qualified HSat.Data.BSP.Common.Clauses as CL
import qualified HSat.Data.BSP.Common.Clause as C (toList)
import qualified HSat.Data.BSP.Common.Literal as L (getWord)

name :: String
name = "Test.Data.BSP.Common.Clauses"

tests :: TestTree
tests = testGroup name [
  testProperty "length correct" test1,
  testProperty "toList . fromList == id" test2,
  testProperty "length == Vector.length" test3,
  testProperty "foldl addClause == id" test4,
  testProperty "maxVariable == max . max" test5
  ]

test1    :: Clauses -> Property
test1 cl = property $ CL.length cl == (length . CL.toList $ cl)

test2    :: Clauses -> Property
test2 cl = property $ cl == (CL.fromList . CL.toList $ cl)

test3    :: Clauses -> Property
test3 cl = property $
  CL.length cl == (fromEnum . length . CL.toList $ cl)

test4    :: Clauses -> Property
test4 cl = property $
  foldl CL.addClause CL.empty (CL.toList cl) == cl

test5    :: Clauses -> Property
test5 cl = property $ CL.maxVariable cl == (
  if CL.isEmpty cl then 0 else
    (maximum . map (\words ->
                 if length words == 0 then 0 else maximum words)
     . map (map L.getWord . C.toList) . CL.toList $ cl))


