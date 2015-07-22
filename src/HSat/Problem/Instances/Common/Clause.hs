{-|
Module      : HSat.Problem.Instances.Common.Clause
Description : The Clause data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the common functions and definition of 'Clause' which is
a collection of 'Literal's

These can be used, for example, to represent a collection of literals
in Conjunctive Normal Form and Disjunctive Normal Form, however are kept deliberately abstract at
this stage. 
-}

module HSat.Problem.Instances.Common.Clause (
  -- * Data Type
  Clause,
  getSizeClause            ,-- :: Clause -> Word
  getVectLiteral           ,-- :: Clause -> Vector Literal
  -- * Construction
  mkClause                 ,-- :: Vector Literal -> Clause
  emptyClause              ,-- :: Clause
  mkClauseFromLits         ,-- :: [Literal] -> Clause
  mkClauseFromIntegers     ,-- :: [Integer] -> Clause
  clauseAddLiteral         ,-- :: Clause -> Literal -> Clause
  -- * Conversions
  clauseToIntegers         ,-- :: Clause -> [Integer]
  -- * Tests
  clauseIsEmpty            ,-- :: Clause -> Bool
  clauseContainsUniqueVars ,-- :: Clause -> Bool 
  ) where

import qualified Data.Set                                      as S
import           Data.Vector                                   (Vector)
import qualified Data.Vector                                   as V
import           HSat.Problem.Instances.Common.Clause.Internal
import           HSat.Problem.Instances.Common.Literal

{-|
Constructs an empty 'Clause'
-}
emptyClause :: Clause
emptyClause = Clause V.empty 0

{-|
Constructs a 'Clause' from a Vector of 'Literal's
-}
mkClause      :: Vector Literal -> Clause
mkClause vect =
  Clause vect (toEnum $ V.length vect)

{-|
Constructs a 'Clause' from a list of 'Literal's
-}
mkClauseFromLits :: [Literal] -> Clause
mkClauseFromLits =
  foldl clauseAddLiteral emptyClause

{-|
Appends the 'Literal' to the 'Clause' and returns the new 'Clause'
-}
clauseAddLiteral                     :: Clause -> Literal -> Clause
clauseAddLiteral (Clause vect n) lit =
  Clause (V.snoc vect lit) (n+1)

{-|
Constructs a list of 'Literal's from a 'Clause'
-}
mkClauseFromIntegers :: [Integer] -> Clause
mkClauseFromIntegers =
  foldl (\clause int ->
          clauseAddLiteral clause $ mkLiteralFromInteger int
        ) emptyClause

{-|
Constructs a list of 'Integer's from a 'Clause'
-}
clauseToIntegers                 :: Clause -> [Integer]
clauseToIntegers (Clause vect _) =
  V.toList . V.map literalToInteger $ vect

{-|
Tests whether a 'Clause' is empty
-}
clauseIsEmpty              :: Clause -> Bool
clauseIsEmpty (Clause _ 0) = True
clauseIsEmpty _            = False

clauseContainsUniqueVars    :: Clause -> Bool
clauseContainsUniqueVars cl =
  let set  = S.fromList . V.toList $ vect
      vect = V.map getVariable $ getVectLiteral cl
  in S.size set == V.length vect
