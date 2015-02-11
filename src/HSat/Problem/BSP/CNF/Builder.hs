{-|
Module      : HSat.Problem.BSP.CNF.Builder
Description : The Builder for the CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The CNF Builder type that builds up CNF instances
-}

module HSat.Problem.BSP.CNF.Builder (
  -- * Safe Construction
  cnfBuilder,
  addLiteral,
  finishClause,
  finalise,
  -- * Unchecked Constructors
  cnfBuilder',
  addLiteral',
  finishClause',
  finalise',
  CNFBuilderError,
  CNFBuildErr,
  CNFBuilder
  ) where

import Data.Word
import HSat.Problem.BSP.CNF.Internal
import HSat.Problem.BSP.CNF.Builder.Internal
import HSat.Problem.BSP.Common

type CNFBuildErr = Either CNFBuilderError CNFBuilder

cnfBuilder :: Word -> Word -> CNFBuildErr
cnfBuilder v c = return $ cnfBuilder' v c 

cnfBuilder' :: Word -> Word -> CNFBuilder
cnfBuilder' v c =
  CNFBuilder v c 0 emptyClauses emptyClause

finishClause :: CNFBuilder -> CNFBuildErr
finishClause cnf = return . finishClause' $ cnf

finishClause' :: CNFBuilder -> CNFBuilder
finishClause' cnf = f $ 
  cnf {
     clauses = clausesAddClause (clauses cnf) (currentClause cnf),
     currentClause = emptyClause
                     }
  where
    f = if clauseIsEmpty . currentClause $ cnf then
          incrClause else
          id

finalise :: CNFBuilder -> Either CNFBuilderError CNF
finalise cnf =
  if (currentClauseNumb cnf) == (expectedClauseNumb cnf) then
    return . finalise' $ cnf else
    Left $ IncorrectClauseNumber (currentClauseNumb cnf) (expectedClauseNumb cnf)

finalise' :: CNFBuilder -> CNF
finalise' (CNFBuilder v max _ cl c) =
  if clauseIsEmpty c then
    CNF v max cl else
    CNF v max $ clausesAddClause cl c

incrClause :: CNFBuilder -> CNFBuilder
incrClause cnf = cnf {currentClauseNumb = (1+) (currentClauseNumb cnf) }

addLiteral :: Literal -> CNFBuilder -> CNFBuildErr
addLiteral l cnf =
  let l' = getWord . getVariable $ l
  in
   if l' == 0 || l' > varNumb cnf then
     Left $ LitOutsideRange l' (varNumb cnf) else
     return . addLiteral' l $ cnf 

addLiteral' :: Literal -> CNFBuilder -> CNFBuilder
addLiteral' l cnf =
  (\cnf' -> cnf' {
    currentClause = clauseAddLiteral (currentClause cnf) l
                    }
            )
  $ if clauseIsEmpty . currentClause $ cnf then
      incrClause cnf else
      cnf
    
