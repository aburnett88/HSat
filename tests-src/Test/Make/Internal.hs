module Test.Make.Internal (
  tests
  ) where

import TestUtils
import HSat.Make.Internal
import Control.Monad.Random
import Control.Monad (liftM)
import Data.Word

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "evalBounded" [
       evalBoundedTest1
       ]
    ]

evalBoundedTest1 :: TestTree
evalBoundedTest1 =
  testProperty "evalBounded (a,b) -> v, v between a and b" $ ioProperty $ do
    (a,b) <- generate genBounded
    let bounds = mkBounds a b
    result <- evalBounded bounds
    let lesser  = getLesser bounds
        greater = getGreater bounds
        gteLeftTest =
          counterexample (show result ++ " < " ++ show lesser)
          (result >= lesser)
        lteRightTest =
          counterexample (show result ++ " > " ++ show greater)
          (result <= greater)
    return . property $ (
      gteLeftTest .&&. lteRightTest
      )

genBounded :: Gen (Word,Word)
genBounded = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)
