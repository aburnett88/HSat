{-# LANGUAGE
    RankNTypes               ,
    ExistentialQuantification,
    MultiParamTypeClasses    ,
    FunctionalDependencies   ,
    OverloadedStrings        ,
    RecordWildCards
  #-}

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

module HSat.Make.Config.Class (
  Config(..),
  Makeable(..),
  mkConfig,
  MakeException(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Random.Class
import Data.Typeable
import HSat.Printer
import HSat.Problem.ProblemExpr.Class
import HSat.Solution.Class

{-|
This super type allows for construction of all types of 'Problem's
-}

class (Eq        config,  Show config   ,
       IsProblem problem, Printer config, Solution problem) =>
      Makeable config problem | config -> problem where
  makeProblem      :: (MonadRandom m, MonadThrow m) =>
                      config -> m (problem, Maybe (SolInstance problem))
                                                                             
data Config = forall config problem. (
  Show config, Typeable config, Makeable config problem) => Config {
  configuration :: config,
  hasProblem    :: Maybe problem
  }


{-|
A simple way to make a config
-}
mkConfig        :: (Typeable config, Makeable config problem) => config -> Config
mkConfig config = Config config Nothing

instance Show Config where
  show (Config c _) = show c

instance Eq Config where
  (Config c _) == (Config c' _) =
    case cast c of
      Just cCast -> cCast == c'
      _ -> False

instance Printer Config where
  compact   = printerConfig Compact
  noUnicode = printerConfig NoUnicode
  unicode   = printerConfig Unicode

printerConfig                  :: PrinterType -> Config -> Doc
printerConfig pType Config{..} =
  preamble <+>
  configDoc
  where
    preamble   :: Doc
    preamble   = (case pType of
      Compact   -> "CONFIG"
      NoUnicode -> "Config"
      Unicode   -> blue "Config"
      ) <> colon
    configDoc  :: Doc
    configDoc  = pTypeToDoc pType configuration

data MakeException = forall exception. (Exception exception) =>
                     MakeException exception

instance Show MakeException where
  show _ = ""

instance Exception MakeException
