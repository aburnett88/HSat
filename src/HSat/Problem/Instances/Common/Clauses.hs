{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : HSat.Problen.Instances.Common.Clauses
Description : The 'Clause' data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the common functions and definitions of 'Clauses' which is
a collection of 'Clause'.

These can be used, for example, to represent a collection of 'Clause' in Conjunctive
Normal Form and Disjunctive Normal Form. Details however are kept abstract
-}
module HSat.Problem.Instances.Common.Clauses (
  -- * Clause
  Clauses,
  getVectClause,           -- :: Clauses -> Vector Clause
  getSizeClauses,          -- :: Clauses -> Word
  -- * Construction
  mkClauses,               -- :: Vector Clause -> Clauses
  mkClausesFromClause,     -- :: [Clause] -> Clauses
  emptyClauses,            -- :: Clauses
  clausesAddClause,        -- :: Clauses -> Clause -> Clauses
  mkClausesFromIntegers,   -- :: [[Integer]] -> Clauses
  -- * Other Functions
  clausesToIntegers,       -- :: Clauses -> [[Integer]]
  clausesIsEmpty,          -- :: Clauses -> Bool
  findMaxVar,              -- :: Clauses -> Word
  getSetOfVars,            -- :: Clauses -> Set Variable
  getSetPos,               -- :: Clauses -> Set Variable
  getSetNeg                -- :: Clauses -> Set Variable
  ) where

import qualified Data.Set                                       as S
import           Data.Vector                                    (Vector)
import qualified Data.Vector                                    as V
import           HSat.Problem.Instances.Common.Clause
import           HSat.Problem.Instances.Common.Clause.Internal
import           HSat.Problem.Instances.Common.Clauses.Internal
import           HSat.Problem.Instances.Common.Literal
import           HSat.Problem.Instances.Common.Sign
import           HSat.Problem.Instances.Common.Variable

{-|
Constructs 'Clauses' from a 'Vector' of 'Clause'
-}
mkClauses      :: Vector Clause -> Clauses
mkClauses vect =
  Clauses vect (toEnum $ V.length vect)

{-|
Constructs an empty 'Clauses'
-}
emptyClauses :: Clauses
emptyClauses = Clauses V.empty 0

{-|
Construct a 'Clauses' from a list of 'Clause'
-}
mkClausesFromClause :: [Clause] -> Clauses
mkClausesFromClause =
  foldl clausesAddClause emptyClauses

{-|
Append the 'Clause' argument to the end of the 'Clauses' and returns the new
'Clauses'
-}
clausesAddClause                  :: Clauses -> Clause -> Clauses
clausesAddClause (Clauses cl n) c =
  Clauses (V.snoc cl c) (n+1)

{-|
Returns 'True' if there are no 'Clause' within the argument
-}
clausesIsEmpty               :: Clauses -> Bool
clausesIsEmpty (Clauses _ 0) = True
clausesIsEmpty _             = False

{-|
Takes a list of list of 'Integer' and constructs a 'Clauses'
-}
mkClausesFromIntegers :: [[Integer]] -> Clauses
mkClausesFromIntegers =
  foldl (\clauses ints ->
          clausesAddClause clauses (mkClauseFromIntegers ints)) emptyClauses

{-|
Constructs a list of lists of 'Integer's from a 'Clauses'
-}
clausesToIntegers :: Clauses -> [[Integer]]
clausesToIntegers =
  V.toList . V.map clauseToIntegers . getVectClause

{-|
A generic method for accumulating information from
each 'Literal' in a 'Clauses'
-}
clFold                              :: (a -> Literal -> a) -> a -> Clauses -> a
clFold function initial Clauses{..} =
  V.foldl (\o Clause{..} ->
            V.foldl function o getVectLiteral
          ) initial $ getVectClause

{-|
Returns the maximum 'Word' represented in a 'Variable within the 'Clauses'
-}
findMaxVar :: Clauses -> Word
findMaxVar =
  clFold (\w Literal{..} ->
           let w' = getWord getVariable
           in if w' > w then
                w' else
                w
                ) 0

{-|
Returns the 'Set' of 'Variable's contained within the argument
-}
getSetOfVars :: Clauses -> S.Set Variable
getSetOfVars =
  clFold (\set Literal{..} ->
           S.insert getVariable set
           ) S.empty

{-|
Returns the 'Set' of all 'Variable's that appear with a positive 'Sign' in the
argument
-}
getSetPos :: Clauses -> S.Set Variable
getSetPos =
  clFold (
    \set Literal{..} ->
     if isPos getSign then
       S.insert getVariable set else
       set
    ) S.empty

{-|
Returns the 'Set' of all 'Variable's that appear with a negative 'Sign' in the
argument
-}
getSetNeg :: Clauses -> S.Set Variable
getSetNeg =
  clFold (
    \set Literal{..} ->
     if isNeg getSign then
       S.insert getVariable set else
       set
    ) S.empty


