{-# LANGUAGE
    ExistentialQuantification,
    RankNTypes
    #-}

{-|
Module      : HSat.Parser.Class
Description : The exports for the 'Parser' class
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

A Class for a 'Parser' to turn text files into 'Problem's
-}

module HSat.Parser.Class (
  Parser(..)
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import HSat.Problem.ProblemExpr.Class

{-|
A existential type that stores Parers's for 'Problem's
-}
data Parser = forall problem. (IsProblem problem) => Parser {
  -- | The file extension of the problem
  fileExtension :: FilePath                                                    ,
  -- | The parser that turns a FilePath into a 'Problem'
  parser        :: forall m. (MonadIO m, MonadThrow m) => FilePath -> m problem
  }
