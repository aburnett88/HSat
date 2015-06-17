{-# LANGUAGE LambdaCase, ExistentialQuantification #-}

{-|
Module      : HSat.Make
Description : Make allows Problem's to be made
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module allows access to a rich set of functions that allow 'Problem's
to be created
-}

module HSat.Make (
  -- * Make
  make,     -- :: (MonadRandom m) => Config -> m Problem
  makeList, -- :: (MonadRandom m) = Int -> Config -> m [Problem]
  -- * Error
  MakeError(..),
  -- * Should be removed
  liftCNF,
  makeProblemFromExpr
  ) where

import Control.Monad (replicateM)
import Control.Monad.Random.Class
import Data.Bifunctor
-- import HSat.Make.BSP.CNF
import HSat.Make.BSP.CNF.Internal (CNFMakeError(..))
import HSat.Make.Config
import HSat.Problem
--import HSat.Problem.BSP.CNF
import HSat.Problem.ProblemExpr
--import HSat.Problem.Source
--import HSat.Problem.BSP
import Control.Monad.Catch

{-|
This data type describes errors that can be thrown when creating 'Problem's.
-}
data MakeError =
  -- | Describes problems' thrown when creating 'CNF' expressions
  CNFError CNFMakeError
  deriving (Eq,Show)

instance Exception MakeError

{-|
Creates a random 'Problem' from a 'Config'.
The Bool argument denotes whether errros are ignored in the Config - e.g.,
some config's may not generate correct problems'. We can mitigate this, and
generate them anyway but without them being poor
-}
make                     :: (MonadRandom m,Applicative m, MonadThrow m) => Config -> Bool ->
                            m Problem
make _ _ = undefined
{-
  second (makeProblemFromExpr config) <$>
  case getInputConfig config of
    CNFProblemType _ -> --cnfConfig ->
      liftCNF <$> if ignoreErrors then
                    undefined else --Right . snd <$> makeCNF' cnfConfig else
                    undefined --makeCNF cnfConfig
-}
makeProblemFromExpr        :: (IsProblem a) => Config -> a -> Problem
makeProblemFromExpr _ = undefined
--  MkProblem (mkMakeConfig config) . changeProblemType (getOutputType config)

{-|
Creates a list of 'Problem's from a given 'Config' and the number required.
We ignore any errors and, if the config is invalid, then the closest valid
config is generated
-}
makeList               :: (MonadRandom m,Applicative m, MonadThrow m) =>
                          Int -> Config -> m [Problem]
makeList number config =
  map (\case
          Left _ -> error "makeList error"
          Right problem -> problem)
  <$> replicateM number (make config True)
