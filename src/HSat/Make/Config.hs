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
import HSat.Printer

{-|
This super type allows for construction of all types of 'Problem's
-}
data Config = Config {
  -- | The output Problem type
  getOutputType :: ProblemType       ,
  -- | The Configuration for creating the original Problem
  getInputConfig :: ConfigProblemType
  } deriving (Eq,Show)

instance Printer Config where
  compact (Config a b) = compact a <+> compact b
  noUnicode (Config a b) = noUnicode a <+> noUnicode b
  unicode (Config a b) = unicode a <+> unicode b

{-|
A supertype that describes all different types of Problem's that
can be created
-}
data ConfigProblemType =
  CNFProblemType CNFConfig
  deriving (Eq,Show)

instance Printer ConfigProblemType where
  compact (CNFProblemType c) = compact c
  noUnicode (CNFProblemType c) = noUnicode c
  unicode (CNFProblemType c) = unicode c

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

instance Printer CNFConfig where
  compact (CNFConfig a b c d e) =
    compact a <+>
    compact b <+>
    compact c <+>
    compact d <+>
    compact e
  noUnicode (CNFConfig a b c d e) =
    noUnicode a <+>
    noUnicode b <+>
    noUnicode c <+>
    noUnicode d <+>
    noUnicode e
  unicode (CNFConfig a b c d e) =
    unicode a <+>
    unicode b <+>
    unicode c <+>
    unicode d <+>
    unicode e

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

instance (Printer a, Printer b) => Printer (Either a b) where
  compact (Left a) = compact a
  compact (Right a) = compact a
  noUnicode (Left a) = noUnicode a
  noUnicode (Right a) = noUnicode a
  unicode (Left a) = unicode a
  unicode (Right a) = unicode a

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

instance Printer PosDouble where
  compact (PosDouble d) = compact d
  noUnicode (PosDouble d) = noUnicode d
  unicode (PosDouble d) = unicode d

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

