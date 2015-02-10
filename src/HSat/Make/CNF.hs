{-|
Module      : HSat.Make.CNF
Description : The evaluator for the Config CNF type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides the ability to create 'Config'urations for randomly generated 'CNF' problems
-}

module HSat.Make.CNF (
  evaluateCNFConfig,
  evaluateCNFConfigErr
  ) where

import           Control.Monad (liftM2,liftM)
import           Control.Monad.Identity
import           Control.Monad.Random
import           Control.Monad.Random.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.Maybe (fromJust)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Make.CNF.Internal
import           HSat.Make.Config
import           HSat.Make.Internal
import           HSat.Problem.BSP.CNF.Internal
import           HSat.Problem.BSP.Common

{-|
A 'CNFConfig' is evaluated. No concern is given to whether it is computable
-}
evaluateCNFConfigErr :: (MonadRandom m) => CNFConfig -> m (Either CNFMakeError CNF)
evaluateCNFConfigErr config =
  runEitherT evaluateConfig'
  where
    evaluateConfig' :: (MonadRandom m) => EitherT CNFMakeError m CNF
    evaluateConfig' = do
      clInit <- evaluateInitErr config
      let nVars = getVarNumb clInit
          nClauses = toEnum . length . getListClSize $ clInit
      clauses <- chooseClauses clInit
      return $ CNF nVars nClauses clauses

evaluateCNFConfig :: (MonadRandom m) => CNFConfig -> m (CNF,CNFConfig)
evaluateCNFConfig config =
  runStateT evaluateConfig' config
  where
    evaluateConfig' :: (MonadRandom m) => StateT CNFConfig m CNF
    evaluateConfig' = do
      clInit <- evaluateInit
      let nVars = getVarNumb clInit
          nClauses = toEnum . length . getListClSize $ clInit
      clauses <- lift $ runEitherT (chooseClauses clInit)
      case clauses of
        Right clauses' -> return $ CNF nVars nClauses clauses'
        _ -> error (show clauses)

evaluateInit :: (MonadRandom m) => StateT CNFConfig m ClausesInit
evaluateInit = do
  numbClauses <- chooseNumbClauses
  (clauseSizes,total) <- chooseEachClauseSize numbClauses
  vars <- chooseNumbVariables total numbClauses
  pred <- gets HSat.Make.Config.getVarPred
  return $ ClausesInit clauseSizes total vars pred

evaluateInitErr :: (MonadRandom m) => CNFConfig -> EitherT CNFMakeError m ClausesInit
evaluateInitErr config = do
  (clausesInit,newConfig) <- runStateT evaluateInit config
  decideUponError config newConfig clausesInit

decideUponError :: (Monad m) => CNFConfig -> CNFConfig -> ClausesInit -> EitherT CNFMakeError m ClausesInit
decideUponError (CNFConfig _ vN vpcN _) (CNFConfig _ vN' vpcN' _) init =
  if vpcN == vpcN' then
    if vN == vN' then
      return $ init else
      left $ UnableToChooseVariables else
    left $ UnableToChooseClauseSizes
