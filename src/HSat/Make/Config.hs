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

module HSat.Make.Config (
  Config(..),
  ConfigProblemType(..),
  CNFConfig(..),
  Bounds,
  VariableNumber,
  ClauseNumber,
  ClauseSizeNumber,
  PosDouble,
  mkPosDouble,
  getDouble,
  VariablePredicate(..)
  ) where

import Data.Word
import HSat.Problem.ProblemType
import HSat.Make.Internal
import System.Random

{-|
This super type allows for construction of all types of 'Problem's
-}
data Config = Config {
  getOutputType :: ProblemType       ,
  getInputConfig :: ConfigProblemType
  } deriving (Eq,Show)

data ConfigProblemType =
  CNFProblemType CNFConfig
  deriving (Eq,Show)

data CNFConfig = CNFConfig {
  getClauseSizeBounds :: ClauseNumber,
  getVariableBounds   :: VariableNumber,
  getClauseSizesBounds :: ClauseSizeNumber,
  getVarsCanAppearTwice :: Bool,
  getDefinitelyHasSolution :: Bool
  } deriving (Eq,Show)

data VariablePredicate =
  NoPredicate |
  AtleastOnce |
  PosAndNeg
  deriving (Eq,Show)

type VariableNumber = Either (Bounds PosDouble) (Bounds Word)

type ClauseNumber = Bounds Word

type ClauseSizeNumber = Bounds Word

newtype PosDouble = PosDouble Double
  deriving (Eq,Show)

mkPosDouble :: Double -> PosDouble
mkPosDouble x
  | x < 0.0 = error "mkPosdouble invalid arg"
  | otherwise = PosDouble x

getDouble :: PosDouble -> Double
getDouble (PosDouble x) = x

instance Bounded PosDouble where
  minBound = PosDouble 0.0
  maxBound = PosDouble . fromIntegral $ (maxBound :: Word)

instance Random PosDouble where
  randomR (PosDouble l,PosDouble r) g =
    let (p,g') = randomR (l,r) g
    in (PosDouble p,g')
  random = randomR (minBound,maxBound)

instance Ord PosDouble where
  compare (PosDouble l) (PosDouble r) = compare l r

