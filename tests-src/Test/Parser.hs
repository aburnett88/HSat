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
        fp' = fp ++ ".cnf"
    didWriteSuceed <- plainProblemToFile problem fp
    if didWriteSuceed then do
      result <- runReadFile $ fromFile fp'
      removeFile fp'
      case result of
        Right problem' -> do
          return $ property $ (mkCNFProblem cnf) === (getProblemExpr problem')
        Left err ->
          return $ counterexample
           ("Unexpected reading error " ++ show err)
           False else
      return $ counterexample
        "File write unsuccessful"
        False
        
fromFileTest1 :: TestTree
fromFileTest1 =
  testProperty "Write random file. Read back in" $ ioProperty $ do
    problem <- generate arbitrary
    let fp = "fromFileTest1"
        fp' = fp ++ ".cnf"
    didWriteSuceed <- plainProblemToFile problem fp
    if didWriteSuceed then do
      result <- runReadFile $ fromFile fp'
      removeFile fp'
      case result of
       Right problem' -> do
         return $ (getProblemExpr problem) === (getProblemExpr problem')
       Left err ->
         return $ counterexample
           ("Failure with error" ++ show err)
           False else
      return $ counterexample "File write unsucessful" False
          
fromFolderTest1 :: TestTree
fromFolderTest1 =
  testProperty "Write random files. Read them all back in" $ ioProperty $ do
    exprs <- generate $ listOf arbitrary
    let folder = "fromFolderTest1"
        problems = map (mkProblem mkStatic) exprs
    result <- writeFolder plainProblemToFile problems "fromFolderTest1"
    if result then do
      problems' <- fromFolder fromFile folder
      let exprs' = map (\a ->
                         case a of
                           Right p -> Right . getProblemExpr $ p
                           Left err -> Left err
                           ) problems'
      removeDirectoryRecursive folder
      return $ listsContainSame (map (\a -> return a) exprs) exprs' else
      return $ counterexample "Folder parsing failed" (False==True)
