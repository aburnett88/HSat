{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

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
import HSat.Problem.ProblemExpr.Class
import Data.Text.IO as T hiding (putStrLn)
import System.Directory
import HSat.Problem.Internal
import HSat.Problem.Instances.CNF.Writer
import Control.Monad.IO.Class

{-|
Writes a 'Problem' to a 'FilePath'.

If the 'Problem' has no file type associated with it, the file is written in CNF

The 'Bool' returned is whether this was sucessful
-}
plainProblemToFile :: (MonadIO m) => Problem -> FilePath -> m (Maybe FilePath)
plainProblemToFile problem fp = plainProblemToFile' (problemExpr problem)
  where
    plainProblemToFile' :: (MonadIO m1) => ProblemExpr -> m1 (Maybe FilePath)
    plainProblemToFile' ProblemExpr{..} = do
      let (text,fileName) = case getWriter expr of
            Nothing ->
              let cnfVersion = toCNF expr
              in (toText cnfVersion, "cnf")
            Just (extension,generatedText) -> (generatedText, makeFileName fp extension)
      fileExists <- liftIO $ doesFileExist fileName
      if fileExists then
        return Nothing else
        liftIO $ T.writeFile fileName text >> return (Just fileName)
    makeFileName :: FilePath -> FilePath -> FilePath
    makeFileName name extension = name ++ "." ++ extension
    
{-|
Given an initial writing function, a list of problems and a folder, writes
all the functions to this folder
-}
writeFolder :: (MonadIO m) => (Problem -> FilePath -> m (Maybe FilePath)) ->
               [Problem] -> FilePath -> String -> m [Maybe FilePath]
writeFolder f problems folder filePrefix = do
  currentDirectory <- liftIO $ getCurrentDirectory 
  liftIO $ createDirectory folder
  liftIO $ setCurrentDirectory folder
  fileNames <- mapM f' $ zip problems ints 
  liftIO $ setCurrentDirectory currentDirectory
  return fileNames
  where
    f' (p,i) = f p (filePrefix ++ show i)
    ints :: [Integer]
    ints = [1..]
