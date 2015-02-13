module Test.Problem.Source (
  tests,
  printer
  ) where

import TestUtils
import HSat.Problem.Source

name :: String
name = "Source"

tests :: TestTree
tests = testGroup name [
  testGroup "mkStatic" [
     mkStaticTest1],
  testGroup "mkFileSource" [
    mkFileSourceTest1]
  ]

printer :: TestTree
printer = testGroup name [
  printSourceArbitrary
  ]

printSourceArbitrary :: TestTree
printSourceArbitrary =
  printTest "Source" (
    (generate arbitrary) :: IO Source)

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
