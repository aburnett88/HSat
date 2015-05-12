
module Test.Writer (
  tests
  ) where

import TestUtils
import qualified Test.Writer.CNF as CNF
import qualified Test.Writer.Internal as Internal
import Data.Either
import HSat.Writer
import HSat.Parser
import Test.Problem ()
import HSat.Problem.Internal
import HSat.Problem.Source
import System.Directory
import HSat.Problem

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
      ],
    Internal.tests,
    CNF.tests
    ]

plainProblemToFileTest1 :: TestTree
plainProblemToFileTest1 =
  testProperty "Write random Problem to file" $ ioProperty $ do
    let fileName = "plainProblemFileTest1"
    problemExpr' <- generate arbitrary
    let problem = mkProblem mkStatic problemExpr'
    success <- plainProblemToFile problem fileName
    let fileName' = createFileName fileName problemExpr'
    if success then do
      returnProblem <- runReadFile $ fromFile fileName'
      removeFile fileName'
      return $ case returnProblem of
        Left err -> counterexample
                    ("Unexpected Error: " ++ show err)
                    False
        Right problem' ->
          problemExpr problem' === problemExpr' else
      return $
        counterexample
        "Could not write file. Exiting"
        False


writeFolderTest1 :: TestTree
writeFolderTest1 =
  testProperty "Write Folder test" $ ioProperty $ do
    let fp = "writeFolderTest1"
    problemExprs <- generate arbitrary
    let problems = map (mkProblem mkStatic) problemExprs
    success <- writeFolder plainProblemToFile problems fp
    if success then do
      returnProblemss <- fromFolder fromFile fp
      let returnProblems = rights returnProblemss
      removeDirectoryRecursive fp
      let problemExprs' = map problemExpr returnProblems
      return $ listsContainSame problemExprs' problemExprs else
      return $ counterexample (
        "writeFolderTest1 failed. Could not sucesfully write to file " ++
        show fp) False
      
