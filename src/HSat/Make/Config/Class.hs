{-# LANGUAGE
  RankNTypes,
  ExistentialQuantification,
  MultiParamTypeClasses,
  FunctionalDependencies
  #-}

{-|
Module      : HSat.Make.Config
Description : The Config module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides the ability to create 'Config'urations for randomly generated
'Problem's
-}

module HSat.Make.Config.Class (
  Config(..),
  Makeable(..),
  ) where

import HSat.Printer
import HSat.Problem.ProblemExpr.Class
import Control.Monad.Random.Class
import Data.Typeable
import Control.Monad.Catch
import HSat.Solution.Class

{-|
This super type allows for construction of all types of 'Problem's
-}

class (Eq config, Show config, IsProblem problem) => Makeable config problem | config -> problem where
  makeProblem :: (MonadRandom m, MonadThrow m) => config -> m problem
  makeNoErrors :: (MonadRandom m) => config -> m (config,problem)
  makeWithSolution :: (MonadRandom m, Solution problem solution) => m (solution, problem)

data Config = forall config problem. (Show config, Typeable config, Makeable config problem) => Config {
  getConfig :: config,
  hasProblem :: Maybe problem
  }

instance Show Config where
  show (Config c _) = show c

instance Eq Config where
  (Config c _) == (Config c' _) =
    case cast c of
      Just cCast -> cCast == c'
      _ -> False

instance Printer Config where
  compact = printerConfig Compact
  noUnicode = printerConfig NoUnicode
  unicode = printerConfig Unicode

printerConfig :: PrinterType -> Config -> Doc
printerConfig = undefined
