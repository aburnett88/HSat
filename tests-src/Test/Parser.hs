{-# LANGUAGE
    LambdaCase
    #-}

{-|
Module      : Test.Parser
Description : The Parser tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the tests for the Parser module
-}

module Test.Parser (
  tests -- :: TestTree
  ) where

import           Control.Monad         (when)
import           HSat.Parser
import           HSat.Problem.Internal
import           HSat.Problem.Source
import           HSat.Writer
import           System.Directory
import qualified Test.Parser.Class      as Class
import           Test.Problem ()
import           TestUtils

name :: String
name = "Parser"

tests :: TestTree
tests =
  testGroup name [
    Class.tests,
    testGroup "fromFile" [
      fromFileTest1
      ],
    testGroup "fromFolder" [
      fromFolderTest1
      ]
    ]

removeFileWithName    :: FilePath -> IO ()
removeFileWithName fp = do
  currentLocation <- getCurrentDirectory
  contents <- getDirectoryContents currentLocation
  let res = filter (\f -> removeSuffix f == fp) contents
  mapM_ removeFile res
  where
    removeSuffix         :: FilePath -> FilePath
    removeSuffix []      = []
    removeSuffix ('.':_) = []
    removeSuffix (x:xs)  = x : removeSuffix xs
        
fromFileTest1 :: TestTree
fromFileTest1 =
  testProperty "Write random file. Read back in" $ monadicIO $ do
    problem <- pick arbitrary
    let fp = "fromFileText1"
    run $ do
      removeFileWithName fp
      (
        \case
          Just fileLocation -> ((===) (problemExpr problem) . problemExpr) <$>
                               fromFile parserInstances fileLocation <* removeFile fileLocation
          Nothing           -> return $ counterexample "Failure to write File" False
        ) =<< plainProblemToFile problem fp

fromFolderTest1 :: TestTree
fromFolderTest1 =
  testProperty "Write random files. Read them all back in" $ monadicIO $ do
    exprs <- pick $ listOf arbitrary
    let folder   = "fromFolderTest1"
        problems = map (MkProblem mkStatic) exprs
        file     = "file"
    run $ do
      flip when (removeDirectoryRecursive folder) =<< doesDirectoryExist folder
      writeFolder plainProblemToFile problems folder file *> (
        listContainsSame' problems <$> fromFolder (fromFile parserInstances) folder
        ) <* removeDirectoryRecursive folder
