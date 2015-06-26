{-# LANGUAGE
    LambdaCase               ,
    ExistentialQuantification,
    RecordWildCards
    #-}

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
  make,     -- :: (MonadRandom m, MonadThrow m, MonadCatch m) => Config -> Bool -> m Problem
  makeList, -- :: (MonadRandom m, MonadThrow m, MonadCatch m) => Int -> Config -> m [Problem]
  Config(..),
  ) where

import Control.Monad                  (replicateM)
import Control.Monad.Catch
import Control.Monad.Random.Class
import HSat.Make.Config.Class
import HSat.Problem
import HSat.Problem.ProblemExpr.Class
import HSat.Problem.Source

{-|
Creates a random 'Problem' from a 'Config'.
The Bool argument denotes whether errors are ignored in the Config - e.g.,
some config's may not generate correct problems'. We can mitigate this, and
generate them anyway but without them being poor
-}
make                                :: (MonadRandom m, MonadThrow m, MonadCatch m) =>
                                       Config -> Bool -> m Problem
make config@Config{..} ignoreErrors = do
  problemExpr <-
    if ignoreErrors then
      snd <$> makeNoErrors configuration else
      makeProblem configuration
  return $ MkProblem (mkMakeConfig config) (ProblemExpr problemExpr)

{-|
Creates a list of 'Problem's from a given 'Config' and the number required.
We ignore any errors and, if the Config is invalid, then the closest valid
Config is generated
-}
makeList               :: (MonadRandom m, MonadThrow m, MonadCatch m) =>
                          Int -> Config -> m [Problem]
makeList number config = replicateM number (make config True)
