{-|
Module      : HSat.Writer
Description : Module for writing Problems to files
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Writes problems
-}

module HSat.Writer (
  plainProblemToFile,
  writeFolder
  ) where

import HSat.Problem

plainProblemToFile :: Problem -> FilePath -> IO Bool
plainProblemToFile _ _ = return False

writeFolder :: (Problem -> FilePath -> IO Bool) -> [Problem] -> FilePath -> IO Bool
writeFolder f ps folder = return False
