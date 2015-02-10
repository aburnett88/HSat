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
  plainProblemToFile
  ) where

import HSat.Problem

plainProblemToFile :: Problem -> FilePath -> IO Bool
plainProblemToFile = undefined
