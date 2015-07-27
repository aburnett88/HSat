{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : Test.Make.Instances.CNF.Internal
Description : Tests for the Internal CNF module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Internal CNF Module
-}

module Test.Make.Instances.CNF.Internal (
  tests               ,-- :: TestTree
  checkBoundsVariables,-- :: Word -> VariableNumber -> Property
  genCNFConfig        ,-- :: Int -> CNFConfig
  ) where

import TestUtils
import HSat.Make.Instances.CNF.Internal
import HSat.Make.Common
import Control.Applicative
import Control.Monad.Catch
import HSat.Make.Config.Class

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkCNFInit" [
       mkCNFInitTest1
       ],
    testGroup "mkCNF" [
      mkCNFTest1
      ]
    ]

mkCNFInitTest1 :: TestTree
mkCNFInitTest1 =
  testProperty "mkCNFInit returns values within bounds" $ monadicIO $ do
    config@CNFConfig{..} <- pick arbitrary
    CNFInit{..} <- run $ mkCNFInit config
    let prop = 
          getWillBeSolvable ===  getWillBeSolvable                                       .&&.
          getVarsCanAppearTwice' === getVarsCanAppearTwice                               .&&.
          checkBoundsVariables getSetMaxVar (toEnum $ length getSizes) getVariableBounds .&&.
          checkBounds (toEnum $ length getSizes) getClauseSizeBounds                     .&&.
          listProperty (`checkBounds` getClauseSizesBounds) getSizes
    stop prop

mkCNFTest1 :: TestTree
mkCNFTest1 =
  testProperty "mkCNF returns CNF that is correct according to argument" $ monadicIO $ do
    cnfInit <- pick arbitrary
    ls <- run $ catch (
      do
        _ <- mkCNF cnfInit
        return $ counterexample "help" False
      )
      (\exception ->
        let _ = show (exception :: MakeException)
        in return $ counterexample "Unexpected exception thrown" False
      )
    stop ls
  
checkBoundsVariables :: Word -> Word -> VariableNumber -> Property
checkBoundsVariables maxVar w vn =
  checkBounds maxVar (case vn of
                          Right set -> set
                          Left double -> f double)
  where
    f     :: Bounds PosDouble -> Bounds Word
    f b =
      let w' = fromRational . toRational $ w :: Double
      in fmap (round . (*) w' . getDouble) b

instance Arbitrary CNFInit where
  arbitrary = sized genCNFInit
  shrink _ = []

genCNFInit :: Int -> Gen CNFInit
genCNFInit i = do
  varsAppearTwice <- arbitrary
  (maxVar,sizes) <- if varsAppearTwice then
                      liftA2 (,) (word 1) (listOf $ word 0) else do
                        sizes <- listOf $ word 0
                        maxVar <- if null sizes then word 1 else choose (maximum sizes, maximum sizes + (toEnum i))
                        return (maxVar,sizes)
  willBeSolvable <- arbitrary
  return $ CNFInit maxVar sizes varsAppearTwice willBeSolvable
  where
    word        :: Word -> Gen Word
    word offSet = (+) offSet . toEnum <$> choose (0,i)
  

instance Arbitrary CNFConfig where
  arbitrary = sized genCNFConfig
  shrink (CNFConfig {}) = []
--    let xs = shrink (a,b,c,d)
 --   in map (\(a,b,c,d) -> CNFConfig a b c d) xs

genCNFConfig   :: Int -> Gen CNFConfig
genCNFConfig _ = do
  clauses <- genBounded 0 (toEnum testMaxClausesSize)
  varsInEach <- genBounded 0 (toEnum testMaxClauseSize)
  vars <-
    oneof [
      Left <$> genBounded (mkPosDouble 0.0) (mkPosDouble 5.0),
      Right <$> genBounded 0 2000
      ]
  posAndNeg <- arbitrary
  x <- arbitrary
  return $ CNFConfig clauses vars varsInEach posAndNeg x

genBounded     :: (Bounded a, Ord a) => a -> a -> Gen (Bounds a)
genBounded l r = return $ mkBounds l r
