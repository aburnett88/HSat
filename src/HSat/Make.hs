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
  MakeError(..)
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

{-|
This data type describes errors that can be thrown when creating 'Problem's.
-}
data MakeError =
  -- | Describes problems' thrown when creating 'CNF' expressions
  CNFError CNFMakeError
  deriving (Eq,Show)


liftCNF :: (ProblemExpr a) => Either CNFMakeError a -> Either MakeError a
liftCNF = bimap CNFError id

{-|
Creates a random 'Problem' from a 'Config'.
The Bool argument denotes whether errros are ignored in the Config - e.g.,
some config's may not generate correct problems'. We can mitigate this, and
generate them anyway but without them being poor
-}
make                     :: (ProblemExpr a, MonadRandom m,Applicative m) => Config -> Bool ->
                            m (Either MakeError (Problem a))
make config ignoreErrors =
  second (makeProblemFromExpr config) <$>
  case getInputConfig config of
    CNFProblemType _ -> --cnfConfig ->
      liftCNF <$> if ignoreErrors then
                    undefined else --Right . snd <$> makeCNF' cnfConfig else
                    undefined --makeCNF cnfConfig

makeProblemFromExpr        :: (ProblemExpr a) => Config -> a -> Problem a
makeProblemFromExpr _ = undefined
--  MkProblem (mkMakeConfig config) . changeProblemType (getOutputType config)

{-|
Creates a list of 'Problem's from a given 'Config' and the number required.
We ignore any errors and, if the config is invalid, then the closest valid
config is generated
-}
makeList               :: (MonadRandom m,Applicative m, ProblemExpr a) =>
                          Int -> Config -> m [Problem a]
makeList number config =
  map (\case
          Left _ -> error "makeList error"
          Right problem -> problem)
  <$> replicateM number (make config True)
