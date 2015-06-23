{-# LANGUAGE
  ExistentialQuantification,
  MultiParamTypeClasses    ,
  RecordWildCards
  #-}

{-|
Module      : HSat.Problem.ProblemExpr
Description : The ProblemExpr type and its associated functions
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Exports the general definition of all 'ProblemExpr's defined
-}

module HSat.Problem.ProblemExpr.Class (
  -- * ProblemExpr
  ProblemExpr(..),
  Convertable(..),
  IsProblem(..),
  fromProblemExpr
  ) where

import Data.Typeable
import HSat.Printer
import Data.Text (Text)
import HSat.Problem.Instances.CNF.Internal

data ProblemExpr = forall s. (IsProblem s) => ProblemExpr {
  expr :: s
  }

instance Printer ProblemExpr where
  compact ProblemExpr{..}   = compact expr
  noUnicode ProblemExpr{..} = noUnicode expr
  unicode ProblemExpr{..}   = unicode expr

instance Eq ProblemExpr where
  (ProblemExpr l) == (ProblemExpr r) =
    case cast l of
      Just l' -> l' == r
      Nothing -> False

fromProblemExpr :: (Typeable s) => ProblemExpr -> Maybe s
fromProblemExpr (ProblemExpr s) = cast s

class (Show problem,
       Printer problem,
       Eq problem,
       Typeable problem) => IsProblem problem where
  getWriter :: problem -> Maybe (FilePath,Text)
  getWriter _ = Nothing
  fromCNF :: CNF -> problem
  toCNF   :: problem -> CNF

class (IsProblem a, IsProblem b) => Convertable a b where
  convert :: a -> b
  convert = fromCNF . toCNF
