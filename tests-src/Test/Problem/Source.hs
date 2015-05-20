module Test.Problem.Source (
  tests
  ) where

import TestUtils
import HSat.Problem.Source
import HSat.Problem.Source.Internal
import Control.Applicative
import HSat.Make.Config
import Test.Make.Config ()

name :: String
name = "Source"

tests :: TestTree
tests = testGroup name [
  testGroup "mkStatic" [
     mkStaticTest1],
  testGroup "mkFileSource" [
    mkFileSourceTest1],
  testGroup "mkMakeConfig" [
    mkMakeConfigTest1]
  ]

mkStaticTest1 :: TestTree
mkStaticTest1 =
  testCase "mkStatic == StaticSource" $ assert (
    mkStatic == StaticSource
    )

mkFileSourceTest1 :: TestTree
mkFileSourceTest1 =
  testProperty "filepath . mkFileSource fp == fp" $ property (
    \filePath ->
    case mkFileSource filePath of
      FileSource filePath' -> filePath == filePath'
      _                    -> False
      )

mkMakeConfigTest1 :: TestTree
mkMakeConfigTest1 =
  testProperty "config . mkMakeConfig c === c" $ property (
    \config ->
    case mkMakeConfig config of
      MakeConfiguration c' -> c' == config
      _ -> False
      )

instance Arbitrary Source where
  arbitrary =
    oneof [
      mkArbStaticSource,
      mkArbFileSource arbitrary,
      mkArbMakeConfig arbitrary
      ]
  shrink StaticSource          = []
  shrink (FileSource filePath) =
    map mkFileSource $ shrink filePath
  shrink (MakeConfiguration config) =
    map mkMakeConfig $ shrink config

mkArbFileSource :: Gen FilePath -> Gen Source
mkArbFileSource = liftA mkFileSource

mkArbStaticSource :: Gen Source
mkArbStaticSource = return StaticSource

mkArbMakeConfig :: Gen Config -> Gen Source
mkArbMakeConfig = liftA mkMakeConfig
