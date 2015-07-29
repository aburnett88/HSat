{-|
Module      : Test.Writer
Description : Tests for the Writer module
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Exports the tests for the Writer module
-}

module Test.Writer (
  tests, -- :: TestTree
  ) where

import HSat.Parser
import HSat.Problem
import HSat.Problem.Internal
import HSat.Problem.ProblemExpr.Class
import HSat.Problem.Source
import HSat.Writer
import System.Directory
import Test.Problem                    ()
import TestUtils

name :: String
name = "Writer"

tests :: TestTree
tests =
  testGroup name [
    testGroup "plainProblemToFile" [
       plainProblemToFileTest1
       ],
    testGroup "writeFolder" [
      writeFolderTest1
      ]
    ]

stripSuffix         :: String -> String
stripSuffix []      = []
stripSuffix ('.':_) = []
stripSuffix (x:xs)  = x : stripSuffix xs

plainProblemToFileTest1 :: TestTree
plainProblemToFileTest1 =
  testProperty "Write random Problem to file" $ monadicIO $ do
    expectedExpr <- pick arbitrary
    let problem  = MkProblem mkStatic expectedExpr
        fileName = "plainProblemFileTest1"
    files        <- run $ getCurrentDirectory >>= getDirectoryContents
    let files'   = filter (\f -> stripSuffix f == fileName) files
    run $ mapM_ removeFile files'
    fileWritten  <- run $ plainProblemToFile problem fileName
    test <- case fileWritten of
     Nothing -> return $ counterexample "File unable to be written" False
     Just fileLocation -> do
       problem' <- run $ fromFile parserInstances fileLocation
       run $ removeFile fileLocation
       let gottenProblem = MkProblem mkStatic (problemExpr problem')
       --Now it gets messy... first check to see if original is equal to file type returned
       if gottenProblem == problem then
         return $ property True else do
           --If not, turn the original to a CNF. Check this against the file returned. 
           let cnfExpr =  convertToCNF . problemExpr $ problem'
           return $ counterexample "Problems not the same" (cnfExpr === expectedExpr)
    stop test
  where
    convertToCNF :: ProblemExpr -> ProblemExpr
    convertToCNF (ProblemExpr e) = ProblemExpr . toCNF $ e

writeFolderTest1 :: TestTree
writeFolderTest1 =
  testProperty "Write Folder test" $ monadicIO $ do
    let folder = "writeFolderTest1"
        file = "file"
    problemExprs <- pick arbitrary
    let problems = map (MkProblem mkStatic) problemExprs
    fileNames    <- run $ writeFolder plainProblemToFile problems folder file
    test <- case compare (length fileNames) (length problems) of
     EQ -> run $ do
       returnProblems    <- fromFolder (fromFile parserInstances) folder
       removeDirectoryRecursive folder
       let problemExprs' = map problemExpr returnProblems
       return $ listsContainSame problemExprs' problemExprs
     _ -> do
       run $ removeDirectoryRecursive folder
       return $ counterexample "Number of files generated not correct" False
    stop test
