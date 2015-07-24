{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : HSat.Make.Instances.CNF.Internal
Description : Exports helper functions for creating CNF problems
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports internal data types and functions for creating CNF files
-}

module HSat.Make.Instances.CNF.Internal (
  -- * Data types
  CNFInit(..)     ,
  CNFConfig(..)   ,
  CNFMakeError(..),
  ClauseNumber    ,
  ClauseSizeNumber,
  VariableNumber  ,
  mkCNFInit       , -- :: (MonadRandom m) => CNFConfig -> m CNFInit
  mkCNF           , -- :: (MonadRandom m, MonadThrow m) => CNFInit -> m CNF
  ) where

import Control.Monad                 (replicateM)
import Control.Monad.Catch
import Control.Monad.Random
import HSat.Make.Common
import HSat.Printer hiding ((<$>))
import HSat.Problem.Instances.CNF
import HSat.Solution.Instances.CNF
import Control.Monad.Trans.State
import HSat.Make.Instances.Common.Literal
import HSat.Make.Instances.Common.Clauses

{-|
CNFInit is a data type that is created that initialises a CNF data type
-}
data CNFInit = CNFInit {
  -- | The maximum 'Variable'
  getSetMaxVar :: Word          ,
  -- | The sizes of the 'Clause'
  getSizes  :: [Word]           ,
  -- | Denotes whether variables can appear twice
  getVarsCanAppearTwice' :: Bool,
  -- | Will the problem be solvable
  getWillBeSolvable :: Bool
  } deriving (Eq,Show)

{-|
The CNFConfig type represents Conjunctive Normal Form Problems
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
  -- | Denotes whether the problem definitely has at least one solution
  getDefinitelyHasSolution :: Bool
  } deriving (Eq,Show)

{-|
Takes a 'CNFConfig' and, in a 'Random' context, creates a 'CNFInit' data
type within the bounds set out by the 'CNFConfig' type.
-}
mkCNFInit                  :: (MonadRandom m) => CNFConfig -> m CNFInit
mkCNFInit (CNFConfig {..}) = do
  numbClauses    <- evalBounded getClauseSizeBounds
  -- The only point of failure
  numbVariables  <- evalVariableNumber numbClauses getVariableBounds
  sizesOfClauses <- replicateM
                      (fromEnum numbClauses) $
                      evalBounded getClauseSizeBounds
  return $ CNFInit
    numbVariables sizesOfClauses
    getVarsCanAppearTwice getDefinitelyHasSolution

evalVariableNumber     :: (MonadRandom m) => Word -> VariableNumber -> m Word
evalVariableNumber x (Left b) =
  (round . (*) (fromIntegral x) . getDouble) <$> evalBounded b
evalVariableNumber _ (Right r) = evalBounded r

{-|
Takes a 'CNFInit' and creates the 'CNF' from it, or throws an error
if it is ill formed
-}
mkCNF   :: (MonadRandom m, MonadThrow m) => CNFInit -> m (CNF, Maybe BoolSolution)
mkCNF CNFInit{..} = do
  literalSet <- mkLiteralSet getSetMaxVar getVarsCanAppearTwice'
  (a,s) <- runStateT (makeClauses getSizes getWillBeSolvable) literalSet
  return $ (mkCNFFromClauses a, if getWillBeSolvable then Just $ getTrueSet s else Nothing)

{-|
A sum-type that describes the types of errors that can be made
-}
data CNFMakeError =
  CNFMakeError
  deriving (Eq,Show)

instance Exception CNFMakeError

{-|
Can either be a Bounded Positive Double that represents a constant multiplier for number
of variable's to clauses, or an exact number of variables
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
