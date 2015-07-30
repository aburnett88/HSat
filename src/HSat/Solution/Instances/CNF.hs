{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies         ,
    RecordWildCards
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
  checkCNFSolution , -- :: CNF -> BoolSolution -> Bool
  emptySolution    , -- :: BoolSolution
  BoolSolution(..) ,
  mkTrueSet        , -- :: (MonadRandom m) => Word -> BoolSolution
  lookup           , -- :: Variable -> BoolSolution -> Maybe Sign
  varsEvalToTrue   ,-- :: Clause -> BoolSolution -> Word
  clausesEvalToTrue,-- :: Clauses -> BoolSolution -> Word
  solutionFromList ,-- :: [Bool] -> BoolSolution
  ) where

import Prelude hiding (lookup)
import Data.Map                      (Map)
import qualified Data.Map            as M
import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.Common
import HSat.Problem.Instances.Common.Clause.Internal
import HSat.Problem.Instances.CNF.Internal
import HSat.Problem.Instances.Common.Clauses.Internal
import HSat.Solution.Class
import Control.Monad.Random
import Control.Monad (replicateM)
import qualified Data.Vector as V

instance Solution CNF where
  type SolInstance CNF = BoolSolution
  checkSolution        = checkCNFSolution

{-|
Checks a CNF against a solution and returns whether it is satisfied or not
-}
checkCNFSolution     :: CNF -> BoolSolution -> Bool
checkCNFSolution CNF{..} bs =
  clausesEvalToTrue getClauses bs == getClauseNumb 
  

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

mkTrueSet :: (MonadRandom m) => Word -> m BoolSolution
mkTrueSet w = do
  BoolSolution . M.fromList . zip varList <$> (replicateM (fromEnum w) getRandom)
  where
    varList = if w == 0 then [] else map mkVariable [1 .. w]

lookup :: Variable -> BoolSolution -> Maybe Sign
lookup w b = M.lookup w $ solution b

clausesEvalToTrue :: Clauses -> BoolSolution -> Word
clausesEvalToTrue Clauses{..} bs =
  V.foldl' clausesEvalToTrue' 0 getVectClause
  where
    clausesEvalToTrue' :: Word -> Clause -> Word
    clausesEvalToTrue' w clause =
      if varsEvalToTrue clause bs > 0 then (w+1) else w

varsEvalToTrue            :: Clause -> BoolSolution -> Word
varsEvalToTrue Clause{..} BoolSolution{..} =
  V.foldl' varsEvalToTrue' 0 getVectLiteral
  where
    varsEvalToTrue' :: Word -> Literal -> Word
    varsEvalToTrue' w Literal{..} =
      let looked = M.lookup getVariable solution
      in case (isPos getSign,looked) of
          (True,Just i) -> if isPos i then (w+1) else w
          (False, Just i) -> if isNeg i then (w+1) else w
          _ -> error "HOWAH: varsEvalToTrue"
  

solutionFromList :: [Bool] -> BoolSolution
solutionFromList list =
  BoolSolution $ solutionFromList' (zip [1..] list) M.empty
  where
    solutionFromList' :: [(Int,Bool)] -> Map Variable Sign -> Map Variable Sign
    solutionFromList' [] m = m
    solutionFromList' ((i,b):xs) m =
      solutionFromList' xs $ M.insert (mkVariable $ toEnum i) (mkSign b) m
