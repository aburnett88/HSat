{-# LANGUAGE
    ExistentialQuantification,
    RankNTypes
    #-}

module HSat.Parser.Class (
  Parser(..)
  ) where

import Control.Monad.Catch
import HSat.Problem.ProblemExpr.Class
import Control.Monad.IO.Class

data Parser = forall problem. (IsProblem problem) => Parser {
  fileExtension :: FilePath,
  parser        :: forall m. (MonadIO m, MonadThrow m) => FilePath -> m problem
  }
