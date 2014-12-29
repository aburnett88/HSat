{-|
Module      : Test.Data.BSP.CNF.Builder
Description : The tests for the CNFBuilder type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the tests for the CNFBuilder type
-}

module Test.Data.BSP.CNF.Builder (
  tests
  ) where

import TestUtils
import Data.Word
import qualified HSat.Data.BSP.Common.Clauses as CL
import HSat.Data.BSP.CNF (CNF(..),fromInts)
import HSat.Data.BSP.CNF.Builder
import qualified HSat.Data.BSP.Common.Literal as L

name :: String
name = "HSat.Data.BSP.CNF.Builder"

tests :: TestTree
tests =
  testGroup name [
    testProperty "cnfBuilder" test1,
    testProperty "finalise after init on non-zero" test2,
    testProperty "test int lists" test3,
    testProperty "test too few clauses" test4,
    testProperty "test too many clauses" test5
    ]

test1 :: (Word,Int) -> Property
test1 (w,i) = 
  let res = cnfBuilder w i >>= \build -> return $
        (_setMaxVar build , _setSize build)
  in
   property $ 
   case res of
     Right (w',i') -> w' == w && i' == i
     Left _ -> False

test2 :: (Word,Int) -> Property
test2 (w,0) = property $ (cnfBuilder w 0 >>= finalise) == (
  Right $ CNF w 0 CL.empty)
test2 (w,i) = property $ (cnfBuilder w i >>= finalise) ==
  (Left $ IncorrectClauseNo i 0)
 
test3 :: [[Int]] -> Property
test3 xs =
  let (w,i) = getFacts (0,0) xs
      start = cnfBuilder w i
  in
   case any (any (==0)) xs of
     True -> property $ go xs start == Left (
       LitOutsideRange (L.fromInt' 0) w)
     False -> property $ go xs start == Right (fromInts xs)

go :: [[Int]] -> Either Error CNFBuilder -> Either Error CNF
go [] cnf = cnf >>= finalise
go (x:xs) cnf = go xs (f x cnf)
  where
    f :: [Int] -> Either Error CNFBuilder -> Either Error CNFBuilder
    f [] c = c >>= finishClause
    f (x:xs) c = f xs $ do
      c' <- c
      c2 <- addLiteral c' x
      return c2

getFacts :: (Word,Int) -> [[Int]] -> (Word,Int)
getFacts a [] = a
getFacts (w,i) (x:xs) = getFacts (foldl f w x,i+1) xs
  where
    f :: Word -> Int -> Word
    f w x
      | w < (toEnum . abs $ x) = (toEnum . abs $ x)
      | otherwise = w

test4 :: [[Int]] -> Property
test4 xs' =
  let xs = map (filter (/=0)) xs'
      half = length xs `div` 2
      (w,i) = getFacts (0,0) xs
  in
   case half < i of
     False -> property $ go xs (cnfBuilder w half) == (
       Right $ fromInts xs)
     True -> property $
       go xs (cnfBuilder w half) == (
         Left $ IncorrectClauseNo half (half+1))

test5 :: [[Int]] -> Property
test5 xs' =
  let xs = map (filter (/=0)) xs'
      half = length xs `div` 2
      (w,i) = getFacts (0,0) xs
  in
   case half == 0 of
     True -> property $ go xs (cnfBuilder w (i+half)) == (
       Right $ fromInts xs)
     False -> property $
       go xs (cnfBuilder w (i+half)) == (
         Left $ IncorrectClauseNo (i+half) i)
