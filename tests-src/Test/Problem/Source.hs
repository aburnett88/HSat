{-|
Module      : Test.Problem.Source
Description : The Source type tests
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Exports all high level tests and internal module tests for the Source type
-}

module Test.Problem.Source (
  tests -- TestTree
  ) where

import           Control.Applicative
import           HSat.Make.Config.Class
import           HSat.Problem.Source
import           HSat.Problem.Source.Internal
import           Test.Make.Config.Class ()
import qualified Test.Problem.Source.Internal as Internal
import           TestUtils

name :: String
name = "Source"

tests :: TestTree
tests = testGroup name [
  Internal.tests,
  testGroup "mkStatic" [
     mkStaticTest1],
  testGroup "mkFileSource" [
    mkFileSourceTest1],
  testGroup "mkMakeConfig" [
    mkMakeConfigTest1]
  ]

incorrectType :: Property
incorrectType = counterexample "Incorrect Source type returned" False

mkStaticTest1 :: TestTree
mkStaticTest1 =
  testCase ("mkStatic " `equiv` " StaticSource") $
   mkStatic @=? StaticSource

mkFileSourceTest1 :: TestTree
mkFileSourceTest1 =
  testProperty ("filepath . mkFileSource fp " `equiv` " fp") $ property
  (\filePath ->
    case mkFileSource filePath of
     FileSource filePath' -> filePath === filePath'
     _                    -> incorrectType
  )

mkMakeConfigTest1 :: TestTree
mkMakeConfigTest1 =
  testProperty ("config . mkMakeConfig c  "`equiv` " c") $ property
  (\config ->
    case mkMakeConfig config of
     MakeConfiguration c' -> c' === config
     _                    -> incorrectType
      )

instance Arbitrary Source where
  arbitrary =
    oneof [
      mkArbStaticSource        ,
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
