{-|
Module      : HSat.Data.BSP.CNF.Builder
Description : The CNF data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The CNF Builder type that builds up CNF instances
-}

module HSat.Data.BSP.CNF.Builder (
  -- * Data Type
  CNFBuilder(..),
  -- * Construction
  cnfBuilder,
  -- * Building Functions
  finishClause,
  addLiteral,
  -- * To CNF Instance
  finalise,
  deconstruct,
  -- * CNF Builder Errors
  Error(..),
  BuildErr
  ) where

import Data.Word
import HSat.Data.BSP.CNF (CNF)
import HSat.Data.BSP.Common.Clause (Clause)
import HSat.Data.BSP.Common.Clauses (Clauses)
import HSat.Data.BSP.Common.Literal (Literal)
import qualified HSat.Data.BSP.Common.Clauses as CL
import qualified HSat.Data.BSP.Common.Clause as C
import qualified HSat.Data.BSP.Common.Literal as L
import qualified HSat.Data.BSP.CNF as CNF

--Order Number P7J9WMDG

{-|
This internal data structure constructs the CNF representation on the fly.

It is included for ease of use and can be used to incrementally build up a
CNF expression.
-}
data CNFBuilder = CNFBuilder {
  _setMaxVar :: Word    ,
  _setSize   :: Int     ,
  _currSize  :: Int     ,
  _clauses   :: Clauses ,
  _clause    :: Clause
  }
  deriving (Show)

{-|
Given a 'Word' and an 'Int', this function is the only method to construct
a CNFBuilder, contained within a 'BuildErr'. 
-}
cnfBuilder :: Word -> Int -> BuildErr
cnfBuilder maxVar size = return $ 
  CNFBuilder maxVar size 0 CL.empty C.empty

incrClause :: CNFBuilder -> BuildErr
incrClause cnf =
  let current = _currSize cnf
  in
   if current < _setSize cnf then
     return $ cnf { _currSize = current + 1 } else
     Left $ IncorrectClauseNo ( _setSize cnf) (current + 1)

{-|
Appends a Literal (given by an Int) to the last clase in a CNF data type
-}
addLiteral :: CNFBuilder -> Int -> BuildErr
addLiteral cnf i =
  (if _clause cnf == C.empty then
     incrClause cnf else
     return cnf
     ) >>= addLiteral' i

--Checks the bounds and adds the literal
addLiteral' :: Int -> CNFBuilder -> BuildErr
addLiteral' i cnf =
  let m = _setMaxVar cnf
      l = L.fromInt' i
  in
   if i== 0 || i > fromEnum m then
     Left $ LitOutsideRange l m else
     return $ cnf { _clause = C.addLit ( _clause cnf) l }

{-|
Finishes the current clause a CNF data type is on
-}
finishClause :: CNFBuilder -> BuildErr
finishClause cnf =
  let cl = CL.addClause ( _clauses cnf) ( _clause cnf)
      newCl = cnf {
        _clauses = cl,
        _clause = C.empty
                  }
  in
   if _clause cnf == C.empty then
     incrClause newCl else
     return newCl

{-|
Performs the final checks and returns a CNF data type. 
-}
finalise :: CNFBuilder -> Either Error CNF
finalise (CNFBuilder max set curr cl c) = 
  if set == curr then
    Right (
      if c == C.empty then
        CNF.CNF max set cl else
        CNF.CNF max set $ CL.addClause cl c
        ) else
    Left $ IncorrectClauseNo set curr
    
{-|
Used for debugging purposes. Easilly convert to a CNF from a CNFBuilder. 
-}
deconstruct :: CNFBuilder -> CNF
deconstruct (CNFBuilder max _ curr cl c) =
  CNF.CNF max curr cl

{-|
The error types that can be thrown from a CNF Builder type. 
-}
data Error =
  LitOutsideRange Literal Word |
  IncorrectClauseNo Int Int |
  OtherError String
  deriving (Eq,Show)

{-|
A type synonym for a CNFBuilder wrapped with an Error type in an Either
-}
type BuildErr = Either Error CNFBuilder
