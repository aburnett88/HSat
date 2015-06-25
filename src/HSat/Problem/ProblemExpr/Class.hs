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

import Data.Text                           (Text)
import Data.Typeable
import HSat.Printer
import HSat.Problem.Instances.CNF.Internal

{-|
A generic data type that can contain data types of the 'IsProblem' class
-}
data ProblemExpr = forall p. (IsProblem p) => ProblemExpr {
  -- | The underlying type that is constrained by 'IsProblem'
  expr :: p
  }

instance Printer ProblemExpr where
  compact   ProblemExpr{..} = compact expr
  noUnicode ProblemExpr{..} = noUnicode expr
  unicode   ProblemExpr{..} = unicode expr

instance Eq ProblemExpr where
  (ProblemExpr l) == (ProblemExpr r) =
    case cast l of
      Just l' -> l' == r
      Nothing -> False

instance Show ProblemExpr where
  show ProblemExpr{..} = show expr

{-|
Takes a 'ProblemExpr' and attempts to construct the original type by casting it
-}
fromProblemExpr                 :: (Typeable s) => ProblemExpr -> Maybe s
fromProblemExpr ProblemExpr{..} = cast expr

{-|
The 'IsProblem' type defines the general problem type in HSat
-}
class (Show problem, Printer  problem,
       Eq   problem, Typeable problem) =>
      IsProblem problem where
  -- | Given an 'IsProblem', may return a tuple of a file extension and the text form of the problem. Default instance is 'Nothing'
  getWriter   :: problem -> Maybe (FilePath,Text)
  getWriter _ = Nothing
  -- | Converts a 'CNF' data type to a 'IsProblem' type
  fromCNF     :: CNF -> problem
  -- | Converts an 'IsProblem' type to a CNF data type
  toCNF       :: problem -> CNF

-- | A class for converting between 'IsProblem' types
class (IsProblem a, IsProblem b) => Convertable a b where
  convert :: a -> b
  convert = fromCNF . toCNF
