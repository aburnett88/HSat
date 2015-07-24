{-# LANGUAGE
    LambdaCase               ,
    ExistentialQuantification,
    RecordWildCards          ,
    ScopedTypeVariables      ,
    MonoLocalBinds
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
  makeList, -- :: (MonadRandom m, MonadThrow m, MonadCatch m) => Int -> Int -> Config -> m [Problem]
  Config(..),
  MakeException(..),
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
                                       Config -> m Problem
make config@Config{..} =
  MkProblem (mkMakeConfig config) . ProblemExpr . fst <$> makeProblem configuration

{-|
Creates a list of 'Problem's from a given 'Config' and the number required.
We ignore any errors and, if the Config is invalid, then the closest valid
Config is generated
-}
makeList                       :: (MonadRandom m, MonadThrow m, MonadCatch m) =>
                                  Int -> Int -> Config -> m [Problem]
makeList number retries config = replicateM number $ makeRetries retries retries config
  where

makeRetries             :: (MonadRandom m, MonadThrow m, MonadCatch m) => Int -> Int -> Config -> m Problem
makeRetries 0 retries _      = throwM $ RetriesException retries
makeRetries n retries config =
  catch (make config) (
    (\e -> do
        let _ = e :: MakeException
        makeRetries (n-1) retries config)
    )     

data RetriesException =
  RetriesException Int
  deriving (Show,Eq)

instance Exception RetriesException

