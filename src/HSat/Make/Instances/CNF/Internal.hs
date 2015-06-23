{-# LANGUAGE RecordWildCards #-}

{-|
Module      : HSat.Make.BSP.CNF.Internal
Description : Exports helper functions for creating CNF problems
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports internal data types and functions for creating CNF files
-}

module HSat.Make.Instances.CNF.Internal (
  -- * Data types
  CNFInit(..),
  CNFConfig(..),
  CNFMakeError(..),
  mkCNFInit,
  mkCNFInit',
  mkCNF,
  mkCNF'
  ) where

import HSat.Make.Config.Class
import HSat.Make.Common
import Control.Monad (replicateM)
import Control.Monad.Random
import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.Common
import HSat.Printer

{-|
CNFInit is a data type that is created that initialises a CNF data type
-}
data CNFInit = CNFInit {
  -- | The maximum 'Variable'
  getSetMaxVar :: Word,
  -- | The sizes of the 'Caluse'
  getSizes  :: [Word],
  -- | Denotes whetehr variabels can appear twice
  getVarsCanAppearTwice' :: Bool,
  -- | Will the problm be solvable
  getWillBeSolvable :: Bool
  } deriving (Eq,Show)

{-|
Takes a 'CNFConfig' and, in a 'Random' context, creates a 'CNFInit' data
type within the bounds set out by the 'CNFConfig' type.
-}
mkCNFInit                  :: (MonadRandom m) => CNFConfig -> m CNFInit
mkCNFInit (CNFConfig {..}) = do
  numbClauses    <- evalBounded getClauseSizeBounds
  numbVariables  <- evalVariableNumber numbClauses getVariableBounds
  sizesOfClauses <- replicateM
                      (fromEnum numbClauses) $
                      evalBounded getClauseSizeBounds
  return $ CNFInit
    numbVariables sizesOfClauses
    getVarsCanAppearTwice getDefinitelyHasSolution

evalVariableNumber :: (MonadRandom m) => Word -> VariableNumber -> m Word
evalVariableNumber x _ = return x

{-|
Takes a 'CNFConfig' and creates a 'CNFInit' with a potentially new 'CNFConfig' if there
were errors in it
-}
mkCNFInit' :: (MonadRandom m) => CNFConfig -> m (CNFConfig,CNFInit)
mkCNFInit' c = do
  initial <- mkCNFInit c
  return (c,initial)

{-|
Takes a 'CNFInit' and creates the 'CNF' from it, or throws an error
if it is ill formed
-}
mkCNF :: (MonadRandom m) => CNFInit -> m (Either CNFMakeError CNF)
mkCNF _ =
  return . Right . mkCNFFromClauses $ emptyClauses

{-|
Will throw a runtime error if there are any errors
-}
mkCNF' :: (MonadRandom m) => CNFInit -> m CNF
mkCNF' initial = do
  result <- mkCNF initial
  return $ case result of
    Left e -> error ("Unexpected error " ++ show e)
    Right cnf -> cnf

{-|
A sumtype that describes the types of errors taht can be made
-}
data CNFMakeError =
  CNFMakeError
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
