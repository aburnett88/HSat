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
  listContainsSame',
  listContainsSame'2',
  equiv              ,
  checkBounds        ,
  listProperty,
  isSame,
  ) where

import qualified Control.Exception as E (catch)
import           Control.Exception.Base (ErrorCall)
import           Control.Monad hiding (liftM,liftM2,liftM3,liftM4,liftM5)
import Control.Applicative
import           Data.Maybe (isNothing)
import           Data.Text (Text,pack)
import           Test.Tasty as Test.Extended
import           Test.Tasty.Golden as Test.Extended
import           Test.Tasty.HUnit as Test.Extended
import           Test.Tasty.QuickCheck as Test.Extended
import           Test.QuickCheck.Monadic as Test.Extended hiding (assert)
import qualified Data.Vector as V
import Data.List (delete)
import qualified Data.Set as S
import HSat.Make.Common

equiv :: String -> String -> String
equiv l r = l ++ "≡" ++ r

--The maximum size a clause is able to be in this configuration
testMaxClauseSize :: Int
testMaxClauseSize = 100

--The maximum size a set of clauses can be in this configuration
testMaxClausesSize :: Int
testMaxClausesSize = 100

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

instance Arbitrary Text where
  arbitrary = do
    x <- arbitrary
    return $ pack x

forceError :: (Eq a, Show a, Arbitrary a) => a -> Assertion
forceError correct = do
  dummyVal  <- generate arbitrary
  maybValue <- E.catch (
    do
      let ans   = Just correct
          dVal' = Just dummyVal
      when (dVal' == ans) (print ans) >> return ans
      )
               (
                 (\_ -> return Nothing)
                 :: ErrorCall -> IO (Maybe a)
               )
  assertBool "Did not throw error" (isNothing maybValue)

propList :: (Show a) => (a -> Property) -> [a] -> Property
propList f list = once $ propList' list
  where
    propList' = foldr (\x -> (.&&.) (
                          counterexample (
                             "Failure on input" ++ show x) (f x)))
                (property True)

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList `liftA` arbitrary
  shrink x =
    map V.fromList $ shrink $ V.toList x

listsContainSame :: (Eq a, Show a) => [a] -> [a] -> Property
listsContainSame [] [] = property True
listsContainSame (x:xs) ys =
  if x `elem` ys then
    listsContainSame xs (delete x ys) else
    counterexample ("Second list does not contain element: " ++ show x ++ (show ys)) False
listsContainSame a b =
  counterexample ("Lists inconsistant" ++ show a ++ show b) False

listContainsSame'2' :: (Ord a, Show a) => [a] -> [a] -> Bool
listContainsSame'2' first second =
  listContainsSame'2'a second
  where
    listContainsSame'2'a [] = True
    listContainsSame'2'a (x:xs) =
      S.member x s && listContainsSame'2'a xs
    s = S.fromList first
      

listContainsSame' :: (Eq a, Show a) => [a] -> [a] -> Bool
listContainsSame' [] [] = True
listContainsSame' (x:xs) ys =
  if x `elem` ys then
    listContainsSame' xs (delete x ys) else
    False
listContainsSame' _ _ = False

listProperty        :: (a -> Property) -> [a] -> Property
listProperty f list = listProperty' list 0
  where
    listProperty' [] _ = property True
    listProperty' (x:xs) i =
      counterexample ("Failure on element " ++ show i) (f x) .&&.
      listProperty' xs ( (i+1) :: Int)

isSame :: [Word] -> [Word] -> Property
isSame [] [] = property True
isSame (x:xs) (y:ys) = x === y .&&. isSame xs ys
isSame _ _ = error "not done"
