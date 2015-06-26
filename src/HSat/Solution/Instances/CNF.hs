{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies
    #-}

{-|
Module      : HSat.Solution.Instances.CNF
Description : Solution instances for CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports functionality for a solution type for CNF
-}

module HSat.Solution.Instances.CNF (
  checkSolution,
  emptySolution,
  BoolSolution(..)
  ) where

import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.Common
import Data.Map (Map)
import qualified Data.Map as M
import HSat.Solution.Class

instance Solution CNF where
  type SolInstance CNF = BoolSolution
  checkSolution = undefined

data BoolSolution = BoolSolution {
  solution :: Map Variable Sign
  } deriving (Eq,Show)

emptySolution :: BoolSolution
emptySolution = BoolSolution M.empty
