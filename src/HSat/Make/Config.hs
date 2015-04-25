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

import HSat.Problem.ProblemType
import HSat.Make.Internal
import System.Random

{-|
This super type allows for construction of all types of 'Problem's
-}
data Config = Config {
  -- | The output Problem type
  getOutputType :: ProblemType       ,
  -- | The Configuration for creating the original Problem
  getInputConfig :: ConfigProblemType
  } deriving (Eq,Show)

{-|
A supertype that describes all different types of Problem's that
can be created
-}
data ConfigProblemType =
  CNFProblemType CNFConfig
  deriving (Eq,Show)

{-|
The CNFConfig type represents Conjucntive Normal Form Problems
-}
data CNFConfig = CNFConfig {
  -- | The number of clauses
  getClauseSizeBounds :: ClauseNumber,
  -- | The number of Variables
  getVariableBounds   :: VariableNumber,
  -- | The sizes of the Clauses
  getClauseSizesBounds :: ClauseSizeNumber,
  -- | Denotes whether Variable's can appear more than once in a clause
  getVarsCanAppearTwice :: Bool,
  -- | Denotes wehther the problem definitely has atleast one solution
  getDefinitelyHasSolution :: Bool
  } deriving (Eq,Show)

{-|
A Variable Predicate 
-}
data VariablePredicate =
  NoPredicate |
  AtleastOnce |
  PosAndNeg
  deriving (Eq,Show)

{-|
Can either be a Bounded Positive Double that represents a constant multiper for number
of variabels to clauses, or an exact nuber of variables
-}
type VariableNumber = Either (Bounds PosDouble) (Bounds Word)

{-|
The number of Clauses is a Bounds word
-}
type ClauseNumber = Bounds Word

{-|
A positive number
-}
type ClauseSizeNumber = Bounds Word

{-|
A newtype wrapper around a Double. 
-}
newtype PosDouble = PosDouble Double
  deriving (Eq,Show)

{-|
The only constructor allows only positive Double's
-}
mkPosDouble :: Double -> PosDouble
mkPosDouble x
  | x < 0.0 = error "mkPosdouble invalid arg"
  | otherwise = PosDouble x

{-|
Gets the Double value from the PosDouble
-}
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

