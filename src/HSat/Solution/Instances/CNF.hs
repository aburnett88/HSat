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
  checkCNFSolution, -- :: CNF -> BoolSolution -> Bool
  emptySolution   , -- :: BoolSolution
  BoolSolution(..), 
  ) where

import Data.Map                      (Map)
import qualified Data.Map            as M
import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.Common
import HSat.Solution.Class

instance Solution CNF where
  type SolInstance CNF = BoolSolution
  checkSolution        = checkCNFSolution

{-|
Checks a CNF against a solution and returns whether it is satisfied or not
-}
checkCNFSolution     :: CNF -> BoolSolution -> Bool
checkCNFSolution _ _ = error "Solution.Instance.CNF:checkNFSolution not written"

{-|
A 'BoolSolution' consists of an internal 'Map' from 'Variable's to 'Sign's
-}
data BoolSolution = BoolSolution {
  solution :: Map Variable Sign
  } deriving (Eq,Show)

{-|
Returns an empty Solution
-}
emptySolution :: BoolSolution
emptySolution = BoolSolution M.empty
