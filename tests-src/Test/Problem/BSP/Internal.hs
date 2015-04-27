module Test.Problem.BSP.Internal (
  tests
  ) where

import TestUtils.Validate
import TestUtils
import HSat.Problem.BSP.Internal
import Test.Problem.BSP.Common.Variable ()
import Control.Applicative

tests :: TestTree
tests = testGroup name [
  ]

name :: String
name = "Internal"

instance Arbitrary BSP where
  arbitrary =
    sized bsp'
    where
      bsp' :: Int -> Gen BSP
      bsp' i
        | i < 1 = oneof [
            liftA Bool' arbitrary,
            liftA Variable' arbitrary
            ]
        | otherwise = oneof [
            do
              i' <- choose (0,i-1)
              n <- bsp' i'
              return $ Not n,
            do
              i1 <- choose (0,i-1)
              i2 <- choose (0,i-1)
              l <- bsp' i1
              r <- bsp' i2
              return $ And l r,
            do
              i1 <- choose (0,i-1)
              i2 <- choose (0,i-1)
              l <- bsp' i1
              r <- bsp' i2
              return $ Or l r,
            do
              i1 <- choose (0,i-1)
              i2 <- choose (0,i-1)
              l <- bsp' i1
              r <- bsp' i2
              return $ If l r,
            do
              i1 <- choose (0,i-1)
              i2 <- choose (0,i-1)
              l <- bsp' i1
              r <- bsp' i2
              return $ IfOnlyIf l r
              ]
  shrink (Variable' v) =
    map Variable' $ shrink v
  shrink (Bool' b) =
    map Bool' $ shrink b
  shrink (Not n) = [n]
  shrink (Or l r) =
    shrink l ++ shrink r
  shrink (And l r) =
    shrink l ++ shrink r
  shrink (If l r) =
    shrink l ++ shrink r
  shrink (IfOnlyIf l r) =
    shrink l ++ shrink r
  
                  
instance Validate BSP where
  validate (Variable' v) = validate v
  validate (Not bsp) = validate bsp
  validate (Or l r) =
    validate l && validate r
  validate (And l r) =
    validate l && validate r
  validate (If l r) =
    validate l && validate r
  validate (IfOnlyIf l r) =
    validate l && validate r
  validate _ = True
