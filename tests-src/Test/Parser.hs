module Test.Parser (
  tests
  ) where

import TestUtils
import HSat.Parser
import HSat.Writer
import HSat.Problem.Internal
import HSat.Problem.Source
import System.Directory
import Control.Monad.Catch
import Test.Problem ()

name :: String
name = "Parser"

tests :: TestTree
tests =
  testGroup name [
    testGroup "fromFile" [
      fromFileTest1
      ],
    testGroup "fromFolder" [
      fromFolderTest1
      ]
    ]

removeFileWithName :: FilePath -> IO ()
removeFileWithName fp = do
  contents <- getDirectoryContents ""
  let res = filter (\f -> (removeSuffix f) == fp) contents
  mapM_ removeFile res
  where
    removeSuffix :: FilePath -> FilePath
    removeSuffix [] = []
    removeSuffix ('.':_) = []
    removeSuffix (x:xs) = x : removeSuffix xs

        
fromFileTest1 :: TestTree
fromFileTest1 =
  testProperty "Write random file. Read back in" $ monadicIO $ do
    problem <- run $ generate arbitrary
    let fp = "fromFileText1"
    run $ removeFileWithName fp
    fileLocation <- run $ plainProblemToFile problem fp
    case fileLocation of
     Just fileLocation' -> do
       resultValue <-
         run $
         catchAll (
           do
             problem' <- fromFile instances fileLocation'
             return $ problemExpr problem === problemExpr problem'
           )
         (\exception -> return $ counterexample ("Exception" ++ show exception) False)
       run $ removeFile fileLocation'
       return resultValue
     Nothing -> return $ counterexample "Failure to write file" False

instances :: [Parser]
instances = error "Unwritten parser instances Test.parser"
          
fromFolderTest1 :: TestTree
fromFolderTest1 =
  testProperty "Write random files. Read them all back in" $ monadicIO $ do
    exprs <- run $ generate $ listOf arbitrary
    let folder = "fromFolderTest1"
        problems = map (MkProblem mkStatic) exprs
        file = "file"
    alreadyExists <- run $ doesDirectoryExist folder
    if alreadyExists then do
      run $ removeDirectoryRecursive folder else return ()
    result <- run $ catchAll (
      do
        _ <- writeFolder undefined problems folder file
        problems' <- fromFolder (fromFile instances) folder
        return $ listsContainSame problems problems'
        )
        (\exception -> return $
                       counterexample ("Exception thrown: " ++ show exception) False)
    run $ removeDirectoryRecursive folder
    return result
