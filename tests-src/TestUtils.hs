{-|
Module      : TestUtils
Description : Commonly used testing functions
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Every commonly used function within the testing framework is exported
from this file. 
-}

module TestUtils (
  module Test.Extended,
  testMaxClauseSize,   -- :: Int
  testMaxClausesSize,  -- :: Int
  mkIntegerNonZero,    -- :: Gen Integer
  mkWordNonZero,       -- :: Gen Word
  checkBounds,         -- :: (Ord a) => a -> Bounds a -> Bool
  testList,
  testEq,
  testAllEq,
  forceError,
  printTest
  ) where

import TestUtils.Problem
import           Control.Monad (when)
import           Data.Maybe (isNothing)
import qualified Control.Exception as E (catch)
import           Control.Exception.Base (ErrorCall)
import           Control.Monad (replicateM,liftM,liftM2)
import           Data.Text (Text,pack,unpack)
import qualified Data.Vector as V
import           Data.Word
import HSat.Make.Config
import HSat.Make.Internal
import           HSat.Problem
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.Common
import           HSat.Problem.ProblemExpr
import qualified HSat.Problem.ProblemType as P
import           HSat.Problem.Source
import           HSat.Writer.Internal
import           System.Random
import           HSat.Writer.CNF
import           HSat.Writer.CNF.Internal
import           Test.Tasty as Test.Extended
import HSat.Printer
import           Test.Tasty.Golden as Test.Extended
import           Test.Tasty.HUnit as Test.Extended
import           Test.Tasty.QuickCheck as Test.Extended
import HSat.Problem.BSP.CNF.Builder.Internal

--The maximum size a clause is able to be in this configuration
testMaxClauseSize :: Int
testMaxClauseSize = 100

--The maximum size a set of clauses can be in this configuration
testMaxClausesSize :: Int
testMaxClausesSize = 100

genBounds :: (Ord a, Bounded a) => Gen a -> Gen (Bounds a)
genBounds f = do
  typ <- choose (0,4) :: Gen Int
  case typ of
    0 -> return mkNoBounds
    1 -> do
      x <- f
      return . mkMinimum $ x
    2 -> do
      x <- f
      return . mkMaximum $ x
    3 -> do
      x <- f
      return . mkExact $ x
    4 -> do
      x <- f
      y <- f
      return $ mkBounds x y

genBounded :: (Ord a, Arbitrary a, Random a, Bounded a) => a -> a -> Gen (Bounds a)
genBounded min max = do
  x <- choose (min,max)
  y <- choose (min,max)
  return $ mkBounds x y

instance (Arbitrary a, Ord a, Bounded a) => Arbitrary (Bounds a) where
  arbitrary = genBounds arbitrary
  shrink bounds = []

instance Arbitrary PosDouble where
  arbitrary = do
    choose (minBound,maxBound)
  shrink d =
    map mkPosDouble . filter (>0.0) $ shrink . getDouble $ d

--Makes sure that the choice is within the bounds of Word so as not to throw errors
mkIntegerNonZero :: Gen Integer
mkIntegerNonZero = do
  let m =  toInteger (maxBound :: Word)
  sign  <- choose (True,False)
  value <- choose (1,m)
  return . (if sign then id else negate) $ value

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
    output <- arbitrary
    input <- arbitrary
    return (Config output input)
  shrink (Config output input) =
    let xs = shrink (output,input)
    in map (\(a,b) -> Config a b) xs

instance Arbitrary ConfigProblemType where
  arbitrary = do
    index <- choose (0,0) :: Gen Int
    case index of
      0 -> do
        cnf <- arbitrary
        return . CNFProblemType $ cnf
  shrink (CNFProblemType cnf) =
    map CNFProblemType $ shrink cnf

instance Arbitrary CNFConfig where
  arbitrary = do
    clauses <- genBounded 0 (toEnum testMaxClausesSize)
    varsInEach <- genBounded 0 (toEnum testMaxClauseSize)
    vars <- do
      index <- choose (0,1) :: Gen Int
      case index of
        0 -> do
          y <-genBounded (mkPosDouble 0.0) (mkPosDouble 5.0)
          return . Left $ y
        1 -> do
          y <- genBounded 0 2000
          return . Right $ y
    posAndNeg <- arbitrary
    return $ CNFConfig clauses vars varsInEach posAndNeg
  shrink (CNFConfig a b c d) = []
--    let xs = shrink (a,b,c,d)
 --   in map (\(a,b,c,d) -> CNFConfig a b c d) xs

testList :: (Show a, Ord a) => Bounds a -> [a] -> Property
testList _ [] = property $ True
testList a (x:xs) = checkBounds x a .&&. testList a xs

testEq :: (Eq a, Show a) => String -> a -> a -> Property
testEq s a b = counterexample (s ++ ": " ++ (show a) ++ " /= " ++ (show b) ++ " ") (a==b)

testAllEq :: (Show a, Eq a) => String -> [a] -> [a] -> Property
testAllEq _ [] [] = property $ True
testAllEq s (x:xs) (y:ys) = (testEq s x y) .&&. testAllEq s xs ys

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
      do
        choose (0,getClauseNumb cnf)
        )
    comments2 <- replicateM noNormalCommentsToAdd arbitrary
    let writer2 = foldl (flip addPreambleComment) writer commentsPreamble
        writer3 = foldl (\aaa (a,b) -> retain aaa a b) writer2 (
          zip commentsClauses comments2)
    return writer3
    


retain :: CNFWriter -> Word -> Comment -> CNFWriter
retain cnf w c =
  let cnf' = addClauseComment w c cnf
  in case cnf' of
    Just res -> res
    Nothing -> cnf

instance Arbitrary Text where
  arbitrary = do
    x <- arbitrary
    return $ pack x

instance Arbitrary VariablePredicate where
  arbitrary = do
    index <- choose (0,2) :: Gen Int
    return $ case index of
      0 -> NoPredicate
      1 -> AtleastOnce
      2 -> PosAndNeg

forceError :: (Eq a, Show a) => a -> a -> Assertion
forceError correct dummyVal = do
  maybValue <- E.catch (
    do
      let ans = Just correct
          dv = Just dummyVal
      when (dv==ans) (print ans) >> return ans
      )
               ((\_ -> return Nothing) :: ErrorCall -> IO (Maybe a))
  assertBool "Did not throw error" (isNothing $ maybValue)

printTest :: (Printer a) => String -> IO a -> TestTree
printTest str getElem =
  testGroup str [
    testCase "Compact" $ do
       elem <- getElem
       putStrLn ""
       putDoc $ compact elem
       putStrLn ""
       assertBool "" True,
    testCase "Non unicode" $ do
      putStrLn ""
      elem <- getElem
      putStrLn ""
      putDoc $ noUnicode elem
      assertBool "" True,
    testCase "Unicode" $ do
      putStrLn ""
      elem <- getElem
      putDoc $ unicode elem
      putStrLn ""
      assertBool "" True
      ]

instance Arbitrary CNFBuilder where
  arbitrary = do
    clauses <- arbitrary
    clause <- arbitrary
    return $ CNFBuilder 0 0 0 clauses clause

instance Arbitrary CNFBuilderError where
  arbitrary = do
    index <- choose (0,1) :: Gen Int
    case index of
      0 -> do
        expected <- choose (0,500)
        return $ IncorrectClauseNumber (expected + 5) expected
      _ -> do
        expected <- choose (0,500)
        return $ LitOutsideRange (expected+5) expected
--348 - 267
