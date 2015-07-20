{-# LANGUAGE
    MultiParamTypeClasses
    #-}

{-|
Module      : HSat.Make.Instances.CNF
Description : High level functions for creating CNF files
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports high level functions for creating 'CNF' files
-}

module HSat.Make.Instances.CNF (
  makeCNF        , -- (MonadRandom m, MonadThrow m) => CNFConfig -> m (CNF, Maybe BoolSolution)
  CNFConfig(..)  , 
  ) where

import Control.Monad.Catch
import Control.Monad.Random
import HSat.Make.Config.Class
import HSat.Make.Instances.CNF.Internal
import HSat.Problem.Instances.CNF
import HSat.Solution.Instances.CNF

instance Makeable CNFConfig CNF where
  makeProblem      = makeCNF

makeCNF :: (MonadRandom m, MonadThrow m) => CNFConfig -> m (CNF, Maybe BoolSolution)
makeCNF = undefined
