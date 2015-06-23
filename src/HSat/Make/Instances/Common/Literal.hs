{-# LANGUAGE RecordWildCards #-}

{-|
Module      : HSat.Make.BSP.Common.Literal
Description : Generic functions to create Literal's
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports a main data type 'LiteralSet' that houses an environment that is used to
create randomly generated 'Literal's with certain criteria - for example, to make
sure that no 'Literal' is created with the same 'Variable' until the context is reset
-}


module HSat.Make.Instances.Common.Literal (
  -- * LiteralSet
  LiteralSet(..),
  mkLiteralSet,
  reset,
  getTrueLiteral,
  getRandomLiteral,
  -- * Type Synonyms
  LiteralMake,
  -- * Errors
  LiteralMakeError(..),
  -- * Depreciated
  LiteralPredicate(..)
  ) where

import           Control.Monad.Random
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           HSat.Problem.Instances.Common

{-|
Provides a context that allows 'Literal's to be created randomly relative to certain
criteria
-}
data LiteralSet = LiteralSet {
  -- | The 'Set' of 'Variable's that are able to appear. If, for example, no 'Variable'
  -- can appear more than once in a context, it is removed from this set
  getVarsThatCanAppear :: Set Variable     ,
  -- | The assignment randomly generated for problem's that must evaluate to 'True'
  getTrueSet           :: Map Variable Sign,
  -- | The number of 'Literal's generated in this context that evaluate to 'True'
  getHasGeneratedTrue  :: Word             ,
  -- | The maximum 'Variable' in the context of contexts. This is used when the context is reset
  getMaximumVariable   :: Word             ,
  -- | Denotes whether a 'Variable' can appear twice before the context is reset
  getVarsAppearTwice   :: Bool
  } deriving (Eq,Show)


{-|
Given a maximum 'Variable' and a 'Bool' denoting whether a 'Variable' can appear twice
in a context, a 'LiteralSet' is created within a 'MonadRandom' context
-}
mkLiteralSet :: (MonadRandom m) => Word -> Bool -> m LiteralSet
mkLiteralSet maxVar vAppearTwice = do
  trueSet <- mkTrueSet
  return $ LiteralSet vars trueSet 0 maxVar vAppearTwice
  where
    vars = S.fromList varList
    varList = if maxVar==0 then [] else map mkVariable [1..maxVar]
    mkTrueSet = M.fromList . zip varList <$> (replicateM (fromEnum maxVar) getRandom)

{-|
When a 'LiteralSet' is generating 'Literal's we allow 'LiteralMakeError's to be
thrown in case of undefined behaviour. 
-}
data LiteralMakeError =
  -- | Thrown when a mapping index cannot be found
  CannotFindMapping |
  -- | Thrown when there are no more 'Variable's allowed to be chosen within a context, though a request for one has been made
  NoVariables
  deriving (Eq,Show)

{-|
A context called 'LiteralMake' that allows for smaller type signatures
-}
type LiteralMake random result =
  StateT LiteralSet (EitherT LiteralMakeError random) result

{-|
Reset's the context within the 'LiteralMake'.

Specifically, puts all 'Variable's back in the 'Variable Set', and resets the number
of Variable's that have evaluated to True back to 0
-}
reset :: (MonadRandom m) => LiteralMake m ()
reset =
  modify reset'
  where
    reset' :: LiteralSet -> LiteralSet
    reset' ls@LiteralSet{..} =
      ls {
        getVarsThatCanAppear = fullSet getMaximumVariable,
        getHasGeneratedTrue  = 0
        }

{-
Creates a full set of Variables from the LiteralSet
-}
fullSet        :: Word -> S.Set Variable
fullSet 0      = S.empty
fullSet maxVar = S.fromList $ map mkVariable [1..maxVar]

{-
Randomly generate a Variable with respect to a context
-}
makeVariable :: (MonadRandom m) => LiteralMake m Variable
makeVariable = do
  vars <- gets getVarsThatCanAppear
  case S.size vars of
    0 -> lift $ left NoVariables
    n -> do
      vAppearTwice <- gets getVarsAppearTwice
      index <- getRandomR (0,n-1)
      let var = S.elemAt index vars
      unless vAppearTwice $ modify $ removeVariable var
      return var

{-
Removes the Variable from the LiteralSet's Variable Set and returns the new LiteralSet
-}
removeVariable :: Variable -> LiteralSet -> LiteralSet
removeVariable v ls@LiteralSet{..} =
  ls {
    getVarsThatCanAppear = S.delete v getVarsThatCanAppear
    }

{-|
Returns a 'Literal' that will evaluate to 'True' within the 'LiteralSet's solution
-}
getTrueLiteral :: (MonadRandom m) => LiteralMake m Literal
getTrueLiteral = do
  var <- makeVariable
  mapping <- gets getTrueSet
  case M.lookup var mapping of
    Nothing -> lift $ left CannotFindMapping
    Just sign -> do
      modify changeTrueLiteralCreated
      return $ mkLiteral sign var

{-
An update function for the LiteralSet data type.
Increases the number of generatedTrue
-}
changeTrueLiteralCreated :: LiteralSet -> LiteralSet
changeTrueLiteralCreated ls@LiteralSet{..} =
  ls {
    getHasGeneratedTrue = getHasGeneratedTrue + 1
  }

{-|
Generates a random literal within the 'LiteralMake' context. 
-}
getRandomLiteral :: (MonadRandom m) => LiteralMake m Literal
getRandomLiteral = do
  var <- makeVariable
  bool <- getRandom
  let sign = mkSign bool
      lit  = mkLiteral sign var
  mapping <- gets getTrueSet
  case M.lookup var mapping of
    Just sign' -> when (sign==sign') (modify changeTrueLiteralCreated) >> return lit
    Nothing -> lift $ left CannotFindMapping

{-|
These are to be depreciated soon, and therefore lack documentation. But they
are used for general things in higher up functions
-}
data LiteralPredicate =
  Any |
  All |
  None
  deriving (Eq,Show)
