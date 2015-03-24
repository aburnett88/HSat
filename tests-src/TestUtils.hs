{-|
Module      : TestUtils
Description : Module node for TestUtils functions
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports commonly used Generator's for testing functions
-}

module TestUtils (
  module Test.Extended, -- Module Export
  testMaxClauseSize,    -- :: Int
  testMaxClausesSize,   -- :: Int
  mkIntegerNonZero,     -- :: Gen Integer
  mkPosIntegerNonZero,  -- :: Gen Integer
  mkNegIntegerNonZero,  -- :: Gen Integer
  mkWordNonZero,        -- :: Gen Word
  testList,             -- ::
  testEq,               -- ::
  testAllEq,            -- ::
  forceError,           -- ::
  propList,              -- :: (a -> Property) -> [a] ->
  listsContainSame,
  ) where

import qualified Control.Exception as E (catch)
import           Control.Exception.Base (ErrorCall)
import           Control.Monad hiding (liftM,liftM2,liftM3,liftM4,liftM5)
import Control.Applicative
import           Data.Maybe (isNothing)
import           Data.Text (Text,pack)
import           Data.Word
import           HSat.Make.Config
import           HSat.Make.Internal
import           HSat.Writer.Internal
import           Test.Tasty as Test.Extended
import           Test.Tasty.Golden as Test.Extended
import           Test.Tasty.HUnit as Test.Extended
import           Test.Tasty.QuickCheck as Test.Extended
import qualified Data.Vector as V
import Data.List (delete)

--The maximum size a clause is able to be in this configuration
testMaxClauseSize :: Int
testMaxClauseSize = 100

--The maximum size a set of clauses can be in this configuration
testMaxClausesSize :: Int
testMaxClausesSize = 100

genBounds :: (Ord a, Bounded a) => Gen a -> Gen (Bounds a)
genBounds f = do
  oneof [
    return mkNoBounds,
    mkMinimum `liftA` f,
    mkMaximum `liftA` f,
    mkExact `liftA` f,
    liftA2 mkBounds f f
    ]



instance (Arbitrary a, Ord a, Bounded a) => Arbitrary (Bounds a) where
  arbitrary = genBounds arbitrary
  shrink _ = []

instance Arbitrary PosDouble where
  arbitrary = choose (minBound,maxBound)
  shrink d =
    map mkPosDouble . filter (>0.0) $ shrink . getDouble $ d

--Makes sure that the choice is within the bounds of Word so as not to throw
-- errors

mkPosIntegerNonZero :: Gen Integer
mkPosIntegerNonZero =
  ((1+) . abs) `liftA` arbitrary

mkNegIntegerNonZero :: Gen Integer
mkNegIntegerNonZero =
  ((-1)*) `liftA` mkPosIntegerNonZero

mkIntegerNonZero :: Gen Integer
mkIntegerNonZero = oneof [
  mkPosIntegerNonZero,
  mkNegIntegerNonZero
  ]

mkWordNonZero :: Gen Word
mkWordNonZero = choose (1,maxBound)

checkBounds :: (Ord a, Show a) => a -> Bounds a -> Property
checkBounds x bound =
  let l = getLesser bound
      g = getGreater bound
  in counterexample (
    show l ++ " <= " ++ show x ++ " <= " ++ show g) $ (x <= g) && (x >= l)
                                                        






testList :: (Show a, Ord a) => Bounds a -> [a] -> Property
testList _ [] = property True
testList a (x:xs) = checkBounds x a .&&. testList a xs

testEq :: (Eq a, Show a) => String -> a -> a -> Property
testEq s a b =
  counterexample (s ++ ": " ++ show a ++ " /= " ++ show b ++ " ") (a==b)

testAllEq :: (Show a, Eq a) => String -> [a] -> [a] -> Property
testAllEq s (x:xs) (y:ys) = testEq s x y .&&. testAllEq s xs ys
testAllEq _ _ _ = property True

instance Arbitrary Comment where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ mkComment x (pack y)

instance Arbitrary Orientation where
  arbitrary = oenof $ map return [Above,Below]

instance Arbitrary Text where
  arbitrary = do
    x <- arbitrary
    return $ pack x

instance Arbitrary VariablePredicate where
  arbitrary =
    oneof $ map return [
      NoPredicate,
      AtleastOnce,
      PosAndNeg
      ]

forceError :: (Eq a, Show a, Arbitrary a) => a -> Assertion
forceError correct = do
  dummyVal <- generate arbitrary
  maybValue <- E.catch (
    do
      let ans = Just correct
          dv = Just dummyVal
      when (dv==ans) (print ans) >> return ans
      )
               ((\_ -> return Nothing) :: ErrorCall -> IO (Maybe a))
  assertBool "Did not throw error" (isNothing maybValue)


--348 - 267--250

propList :: (Show a) => (a -> Property) -> [a] -> Property
propList f list = once $ propList' list
  where
    propList' [] = property True
    propList' (x:xs) =
      (counterexample ("Failed on input " ++ show x) (f x)) .&&. propList' xs

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList `liftA` arbitrary
  shrink x =
    map V.fromList $ shrink $ V.toList x

listsContainSame :: (Eq a, Show a) => [a] -> [a] -> Property
listsContainSame [] [] = property True
listsContainSame (x:xs) ys =
  if elem x ys then
    listsContainSame xs (delete x ys) else
    counterexample ("Second list does not contain element" ++ show x) False
listsContainSame a b =
  counterexample ("Lists inconsistant" ++ (show a) ++ (show b)) False
      
