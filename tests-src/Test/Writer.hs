
module Test.Writer (
  tests
  ) where

import           Data.Either
import           HSat.Parser
import           HSat.Problem
import           HSat.Problem.Internal
import           HSat.Problem.ProblemExpr
import           HSat.Problem.Source
import           HSat.Writer
import           System.Directory
import           Test.Problem ()
import qualified Test.Writer.CNF as CNF
import qualified Test.Writer.Internal as Internal
import           TestUtils
import HSat.Problem.ProblemType as T

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
    testGroup "createFileName" [
      createFileNameTest1
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
    if success then
      case fileName' of
        Nothing -> do
          let problemInCnf = mkCNFProblem $ problemToCNF problemExpr'
              fn = fileName ++ ".cnf"
          returnProblem <- runReadFile $ fromFile fn
          removeFile fn
          return $ case returnProblem of
            Left err -> counterexample ("Unexpected error: " ++ show err) False
            Right problem' -> problemExpr problem' === problemInCnf
        Just f -> do
          returnProblem <- runReadFile $ fromFile f
          removeFile f
          return $ case returnProblem of
            Left err -> counterexample ("Unexpected error: " ++ show err) False
            Right problem' -> problemExpr problem' === problemExpr' else
      return $ counterexample "Could not write file. Exiting..." False


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
      print problemExprs'
      print problemExprs
      return $ listsContainSame problemExprs' problemExprs else
      return $ counterexample (
        "writeFolderTest1 failed. Could not sucesfully write to file " ++
        show fp) False
      
createFileNameTest1 :: TestTree
createFileNameTest1 =
  testProperty "createFileName returns correct output" $ property
  (\(fp,expr) ->
    let exptd = case problemType expr of
          T.CNF -> Just $ fp ++ ".cnf"
          _ -> Nothing
        gotten = createFileName fp expr
    in exptd === gotten
  )
