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
  propList              -- :: (a -> Property) -> [a] -> Property
  ) where

import qualified Control.Exception as E (catch)
import           Control.Exception.Base (ErrorCall)
import           Control.Monad
import           Data.Maybe (isNothing,fromMaybe)
import           Data.Text (Text,pack)
import           Data.Word
import           HSat.Make.Config
import           HSat.Make.Internal
import           HSat.Problem.BSP.CNF
import           HSat.Writer.CNF
import           HSat.Writer.CNF.Internal
import           HSat.Writer.Internal
import           System.Random
import           Test.Tasty as Test.Extended
import           Test.Tasty.Golden as Test.Extended
import           Test.Tasty.HUnit as Test.Extended
import           Test.Tasty.QuickCheck as Test.Extended
import           TestUtils.Problem ()

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
    mkMinimum `liftM` f,
    mkMaximum `liftM` f,
    mkExact `liftM` f,
    liftM2 mkBounds f f
    ]

genBounded :: (Ord a, Arbitrary a, Random a, Bounded a) =>
              a -> a -> Gen (Bounds a)
genBounded min' max' = do
  x <- choose (min',max')
  y <- choose (min',max')
  return $ mkBounds x y

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
  ((1+) . abs) `liftM` arbitrary

mkNegIntegerNonZero :: Gen Integer
mkNegIntegerNonZero =
  ((-1)*) `liftM` mkPosIntegerNonZero

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
                                                        
instance Arbitrary Config where
  arbitrary = do
    out <- arbitrary
    input <- arbitrary
    return (Config out input)
  shrink (Config out input) =
    let xs = shrink (out,input)
    in map (uncurry Config) xs

instance Arbitrary ConfigProblemType where
  arbitrary = do
    oneof [
      CNFProblemType `liftM` arbitrary
      ]
  shrink (CNFProblemType cnf) =
    map CNFProblemType $ shrink cnf

instance Arbitrary CNFConfig where
  arbitrary = do
    clauses <- genBounded 0 (toEnum testMaxClausesSize)
    varsInEach <- genBounded 0 (toEnum testMaxClauseSize)
    vars <- do
      oneof [
        (Left) `liftM` genBounded (mkPosDouble 0.0) (mkPosDouble 5.0),
        (Right) `liftM` genBounded 0 2000
        ]
    posAndNeg <- arbitrary
    x <- arbitrary
    return $ CNFConfig clauses vars varsInEach posAndNeg x
  shrink (CNFConfig _ _ _ _ _) = []
--    let xs = shrink (a,b,c,d)
 --   in map (\(a,b,c,d) -> CNFConfig a b c d) xs

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
    return $ Comment x (pack y)

instance Arbitrary Orientation where
  arbitrary = do
    x <- choose (0,1) :: Gen Int
    case x of
      0 -> return Above
      _ -> return Below

instance Arbitrary CNFWriter where
  arbitrary = do
    cnf <- arbitrary
    let writer = mkCNFWriter cnf
    noPreCommentsToAdd <- choose (0,100) :: Gen Int
    commentsPreamble <- replicateM noPreCommentsToAdd arbitrary
    noNormalCommentsToAdd <- choose (0,100) :: Gen Int
    commentsClauses <- replicateM noNormalCommentsToAdd (
      choose (0,getClauseNumb cnf))
    comments2 <- replicateM noNormalCommentsToAdd arbitrary
    let writer2 = foldl (flip addPreambleComment) writer commentsPreamble
        writer3 = foldl (\aaa (a,b) -> retain aaa a b) writer2 (
          zip commentsClauses comments2)
    return writer3
    
retain :: CNFWriter -> Word -> Comment -> CNFWriter
retain cnf w c =
  let cnf' = addClauseComment w c cnf
  in fromMaybe cnf cnf'

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
