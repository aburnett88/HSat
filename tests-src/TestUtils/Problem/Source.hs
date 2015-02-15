module TestUtils.Problem.Source (
  mkArbFileSource,
  mkArbStaticSource
  ) where

import HSat.Problem.Source
import TestUtils.Test

instance Arbitrary Source where
  arbitrary = do
    typeOf <- choose (0,1) :: Gen Int
    case typeOf of
      0 -> mkArbStaticSource
      _ -> mkArbFileSource arbitrary
  shrink StaticSource          = []
  shrink (FileSource filePath) =
    map mkFileSource $ shrink filePath

mkArbFileSource :: Gen FilePath -> Gen Source
mkArbFileSource = liftM mkFileSource

mkArbStaticSource :: Gen Source
mkArbStaticSource = return StaticSource
