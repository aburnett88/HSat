module Test.Parser (
  tests
  ) where

import TestUtils
import qualified Test.Parser.CNF as CNF
import HSat.Parser
import HSat.Writer
import HSat.Problem
import HSat.Problem.Source
import HSat.Problem.ProblemExpr
import System.Directory

name :: String
name = "Parser"

tests :: TestTree
tests =
  testGroup name [
    CNF.tests,
    testGroup "fromCNFFile" [
      fromCNFFileTest1
      ],
    testGroup "fromFile" [
      fromFileTest1
      ],
    testGroup "fromFolder" [
      fromFolderTest1
      ]
    ]

fromCNFFileTest1 :: TestTree
fromCNFFileTest1 =
  testProperty "Write CNF file, read back in" $ ioProperty $ do
    cnf <- generate arbitrary
    let problem = mkProblem mkStatic $ mkCNFProblem cnf
        fp = "fromCNFFileTest1"
    didWriteSuceed <- plainProblemToFile problem fp
    if didWriteSuceed then do
      result <- fromFile (fp ++ ".cnf")
      removeFile (fp ++ ".cnf")
      return $ case result of
       Right problem' -> property $ problem === problem'
       _ -> property False else
      return $ counterexample "File write unsucessful" $ False===True
        
fromFileTest1 :: TestTree
fromFileTest1 =
  testProperty "Write random file. Read back in" $ ioProperty $ do
    problem <- generate arbitrary
    let fp = "fromFileTest1"
    plainProblemToFile problem fp
    result <- fromFile (fp ++ ".cnf")
    removeFile (fp ++ ".cnf")
    return $ case result of
      Right problem' -> problem === problem'
      _ -> property False

fromFolderTest1 :: TestTree
fromFolderTest1 =
  testProperty "Write random files. Read them all back in" $ ioProperty $ do
    problems <- generate $ listOf arbitrary
    let folder = "fromFolderTest1"
    result <- writeFolder plainProblemToFile problems "fromFolderTest1"
    if result then do
      problems' <- fromFolder fromFile folder
      list <- getDirectoryContents folder
      mapM_ removeFile list
      removeDirectory folder
      return $ property (False === True) else
      return $ counterexample "Folder parsing failed" (False==True)
