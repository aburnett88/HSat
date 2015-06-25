
module Test.Writer (
  tests
  ) where

import           HSat.Parser
import           HSat.Problem
import           HSat.Problem.Internal
import           HSat.Problem.Source
import           HSat.Writer
import           System.Directory
import           Test.Problem ()
import           TestUtils
import HSat.Problem.ProblemExpr.Class

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

stripSuffix :: String -> String
stripSuffix [] = []
stripSuffix ('.':_) = []
stripSuffix (x:xs) = x : stripSuffix xs

plainProblemToFileTest1 :: TestTree
plainProblemToFileTest1 =
  testProperty "Write random Problem to file" $ monadicIO $ do
    let fileName = "plainProblemFileTest1"
    expectedExpr <- pick arbitrary
    let problem = MkProblem mkStatic expectedExpr
    exists <- run $ getDirectoryContents ""
    let files = filter (\f -> (stripSuffix f) == fileName) exists
    run $ mapM_ removeFile files
    fileWritten <- run $ plainProblemToFile problem fileName
    case fileWritten of
     Nothing -> return $ counterexample "File unable to be written" False
     Just fileLocation -> do
       problem' <- run $ fromFile [] fileLocation
       let gottenProblem = MkProblem mkStatic (problemExpr problem')
       --Now it gets messy... first check to see if original is equal to filetype returned
       if gottenProblem == problem then
         return $ counterexample "" True else do
           --If not, turn the original to a CNF. Check this against the file returned. 
           let cnfExpr =  convertToCNF . problemExpr $ problem'
           return $ counterexample "Problems not the same" (cnfExpr === expectedExpr)
  where
    convertToCNF :: ProblemExpr -> ProblemExpr
    convertToCNF = undefined

writeFolderTest1 :: TestTree
writeFolderTest1 =
  testProperty "Write Folder test" $ monadicIO $ do
    let folder = "writeFolderTest1"
        file = "file"
    problemExprs <- pick arbitrary
    let problems = map (MkProblem mkStatic) problemExprs
    fileNames <- run $ writeFolder plainProblemToFile problems folder file
    case compare (length fileNames) (length problems) of
     EQ -> run $ do
       returnProblems <- fromFolder (fromFile []) folder
       removeDirectoryRecursive folder
       let problemExprs' = map problemExpr returnProblems
       return $ listsContainSame problemExprs' problemExprs
     _ -> do
       run $ removeDirectoryRecursive folder
       return $ counterexample "Number of files generated not correct" False
