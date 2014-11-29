{-|
Module      : TestUtils
Description : Used to export common testing functions
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Basically exports everything we commonly use for testing HSat.
If it appears in more than one Testing file, it should go
here. 
-}

module TestUtils (
  module Test.Tasty,
  module Test.Tasty.QuickCheck,
  module Test.Tasty.HUnit,
  module Test.Tasty.Golden,
  props,
  cases
  ) where

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import qualified HSat.Data.BSP.Common.Literal as L
import qualified HSat.Data.BSP.Common.Sign as S
import qualified HSat.Data.BSP.Common.Variable as V
import qualified HSat.Data.BSP.Common.Clause as C
import qualified HSat.Data.BSP.Common.Clauses as CL
import qualified HSat.Data.BSP.CNF as CNF
import qualified Data.Vector as VE

instance Arbitrary CNF.CNF where
  arbitrary = do
    cl <- arbitrary
    return . CNF.fromClauses $ cl

instance Arbitrary CL.Clauses where
  arbitrary = do
    x <- choose (0,50)
    v <- vectorOf x arbitrary
    return $ CL.fromList v

instance Arbitrary C.Clause where
  arbitrary = do
    x <- choose (0,100)
    v <- vectorOf x arbitrary
    return $ C.Clause (VE.fromList v)

instance Arbitrary S.Sign where
  arbitrary = do
    t <- choose (True,False)
    return $ S.fromBool t

instance Arbitrary V.Variable where
  arbitrary = do
    v <- choose (1,1000)
    return $ V.fromWord v

instance Arbitrary L.Literal where
  arbitrary = do
    s <- arbitrary
    v <- arbitrary
    return $ L.Literal s v

props :: String -> String
props name = name ++ " Properties"

cases :: String -> String
cases name = name ++ " Test Cases"

