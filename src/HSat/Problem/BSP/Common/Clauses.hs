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

import qualified Data.Set as S
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import           HSat.Problem.BSP.Common.Clause
import           HSat.Problem.BSP.Common.Clauses.Internal
import           HSat.Problem.BSP.Common.Literal
import           HSat.Problem.BSP.Common.Sign
import           HSat.Problem.BSP.Common.Variable

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
A generic method for accumilating information from
each 'Literal' in a 'Clauses'
-}
clFold                          :: (a -> Literal -> a) -> a -> Clauses -> a
clFold function initial clauses =
  V.foldl (\o cl ->
            V.foldl function o $ getVectLiteral cl
          ) initial $ getVectClause clauses

{-|
Returns the maximum 'Word' represented in a 'Variab'e within the 'Clauses'
-}
findMaxVar :: Clauses -> Word
findMaxVar =
  clFold (\w lit ->
           let w' = getWord $ getVariable lit
           in if w' > w then
                w' else
                w
                ) 0

{-|
Returns the 'Set' of 'Variable's contained within the argument
-}
getSetOfVars :: Clauses -> S.Set Variable
getSetOfVars =
  clFold (\set lit ->
           S.insert (getVariable lit) set
           ) S.empty

{-|
Returns the 'Set' of all 'Variable's that appear with a positive 'Sign' in the
argument
-}
getSetPos :: Clauses -> S.Set Variable
getSetPos =
  clFold (\set l ->
           if isPos . getSign $ l then
              S.insert (getVariable l) set else
              set
         ) S.empty

{-|
Returns the 'Set' of all 'Variable's that appear with a negative 'Sign' in the
argument
-}
getSetNeg :: Clauses -> S.Set Variable
getSetNeg =
  clFold (\set l ->
           if isNeg . getSign $ l then
             S.insert (getVariable l) set else
             set
         ) S.empty
