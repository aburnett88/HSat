{-|
Module      : HSat.Writer
Description : High level module to write Problems to Files
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides functionality for writing 'Problem's to files
-}

module HSat.Writer (
  plainProblemToFile,
  writeFolder
  ) where

import HSat.Problem
import HSat.Problem.ProblemExpr.Internal
import Data.Text.IO as T hiding (putStrLn)
import HSat.Writer.CNF
import System.Directory
import Control.Monad (foldM)
import HSat.Problem.Internal

{-|
Writes a 'Problem' to a 'FilePath'.

If the 'Problem' has no file type associated with it, the file is written in CNF

The 'Bool' returned is whether this was sucessful
-}
plainProblemToFile :: (ProblemExpr a) => Problem a -> FilePath -> IO Bool
plainProblemToFile problem fp = do
  let (text,fileName) = case getWriterInfo $ problemExpr problem of
        Nothing ->
          let cnfVersion = toCNF $ problemExpr problem
          in (runCNFWriter $ mkCNFWriter cnfVersion, makeFileName fp "cnf")
        Just (extension,generatedText) ->
          (generatedText,makeFileName fp extension)
  exists <- doesFileExist fileName
  if exists then
    return False else
    T.writeFile fileName text >> return True
  where
    makeFileName :: FilePath -> FilePath -> FilePath
    makeFileName name extension = name ++ "." ++ extension
    
{-|
Given an initial writing function, a list of problems and a folder, writes
all the functions to this folder
-}
writeFolder :: (ProblemExpr a) => (Problem a -> FilePath -> IO Bool) ->
               [Problem a] -> FilePath -> IO Bool
writeFolder f problems folder = do
  folderExists <- doesDirectoryExist folder
  if folderExists then
    return False else do
      createDirectory folder
      setCurrentDirectory folder
      returnValue <- foldM f' True (zip problems ints)
      setCurrentDirectory ".."
      return returnValue
  where
    f' False _ = return False
    f' _ (p,i) = f p ("file" ++ show i)
    ints :: [Integer]
    ints = [1..]
