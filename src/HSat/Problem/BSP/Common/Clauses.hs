{-|
Module      : HSat.Problen.BSP.Common.Clauses
Description : The 'Clause' data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

A 'Clauses' data type represents a collection of 'Clause'
-}
module HSat.Problem.BSP.Common.Clauses (
  -- * Clause
  Clauses,
  getVectClause,           -- :: Clauses -> Vector Clause
  getSizeClauses,          -- :: Clauses -> Word
  -- * Construction
  mkClauses,               -- :: Vector Clause -> Clauses
  mkClausesFromClause,     -- :: [Clause] -> Clauses
  emptyClauses,             -- :: Clauses
  clausesAddClause,        -- :: Clauses -> Clause -> Clauses
  mkClausesFromIntegers,   -- :: [[Integer]] -> Clauses
  -- * Other Functions
  clausesToIntegers,       -- :: Clauses -> [[Integer]]
  clausesIsEmpty,          -- :: Clauses -> Bool
  findMaxVarAndSizeClauses,-- :: Clauses -> (Word,Word)
  getSetOfVars,            -- :: Clauses -> Set Variable
  getSetPos,               -- :: Clauses -> Set Variable
  getSetNeg                -- :: Clauses -> Set Variable
  ) where

import qualified Data.Set as S
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import           Data.Word
import           HSat.Printer
import           HSat.Problem.BSP.Common.Clause
import HSat.Problem.BSP.Common.Literal
import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Common.Sign
import HSat.Problem.BSP.Common.Clauses.Internal

{-|
Constructs 'Clauses' from a 'Vector' of 'Clause'
-}
mkClauses :: Vector Clause -> Clauses
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
Append the 'Clause' to the 'Clauses' to create a new 'Clauses'
-}
clausesAddClause                  :: Clauses -> Clause -> Clauses
clausesAddClause (Clauses cl n) c =
  Clauses (V.snoc cl c) (n+1)

{-|
Returns 'True' if the 'Clauses' has no elements within
-}
clausesIsEmpty               :: Clauses -> Bool
clausesIsEmpty (Clauses _ 0) = True
clausesIsEmpty _             = False

{-|
Takes a list of list of 'Integer' and constructs a 'Clauses'.

This will throw a runtime error if any of the 'Integer's are either zero, or outside the range of
supported values, which is constrained by the 'Word' data type. 
-}
mkClausesFromIntegers :: [[Integer]] -> Clauses
mkClausesFromIntegers =
  foldl (\clauses ints ->
          clausesAddClause clauses (mkClauseFromIntegers ints)) emptyClauses

{-|
Constructs a list of list of 'Integer' that represents the 'Clauses' argument
-}
clausesToIntegers :: Clauses -> [[Integer]]
clausesToIntegers =
  V.toList . V.map clauseToIntegers . getVectClause

{-|
Returns a tuple containing the maximum 'Variabe' within a set of 'Clauses' and the
length of those 'Clauses'
-}
findMaxVarAndSizeClauses :: Clauses -> (Word,Word)
findMaxVarAndSizeClauses =
  --Fold over each clause
  V.foldl computeVarPlusClause (0,0) . getVectClause
  where
    --Add one for each clause encoutered, find the maximum var as we proceed
    computeVarPlusClause :: (Word,Word) -> Clause -> (Word,Word)
    computeVarPlusClause (maxVar,clause) c = (maxVar',clause+1)
      where
        maxVar' = V.foldl (
          \current newLit ->
          let potentialMax = getWord . getVariable $ newLit
          --compare current maxvar and potentially new max var
          in case compare current potentialMax of
            LT -> potentialMax
            _ -> current
            ) maxVar . getVectLiteral $ c

getSetOfVars :: Clauses -> S.Set Variable
getSetOfVars = generalFold (\set l -> S.insert (getVariable l) set)

generalFold :: (S.Set Variable -> Literal -> S.Set Variable) ->
               Clauses -> S.Set Variable
generalFold f (Clauses cl _) =
  V.foldl (\set clause -> get set clause) S.empty cl
  where
    get s c = V.foldl f s (getVectLiteral c)

getSetPos :: Clauses -> S.Set Variable
getSetPos = generalFold (
  \set l -> if isPos . getSign $ l then
              S.insert (getVariable l) set else
              set
              )

getSetNeg :: Clauses -> S.Set Variable
getSetNeg = generalFold (
  \set l -> if isNeg . getSign $ l then
              S.insert (getVariable l) set else
              set
              )
