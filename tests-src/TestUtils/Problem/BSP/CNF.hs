module TestUtils.Problem.BSP.CNF (
  genCNF,
  genCNFIncorrectClauses,
  genCNFIncorrectVarNumb
  ) where

import TestUtils.Problem.BSP.CNF.Builder
import TestUtils.Test
import HSat.Problem.BSP.CNF
import HSat.Problem.BSP.Common.Clauses
import TestUtils.Problem.BSP.Common.Clauses
import TestUtils.Problem.BSP.Common.Clause
import TestUtils.Problem.BSP.Common.Sign
import TestUtils.Problem.BSP.Common.Literal
import TestUtils.Problem.BSP.Common.Variable
import HSat.Problem.BSP.CNF.Internal
import HSat.Problem.BSP.Common
import Control.Monad (liftM3)
import Data.Word
import qualified Data.Vector as V

genCNF :: Word -> Word -> Word -> Word -> Gen CNF
genCNF maxVar clauseSize clausesSize varOffset = do
  clauses <- genClauses maxVar clauseSize clausesSize
  maxVar' <- choose (findMaxVar clauses,maxVar)
  return $ CNF maxVar' (getSizeClauses clauses) clauses

genCNFIncorrectClauses :: Word -> Word -> Word -> Word -> Gen CNF
genCNFIncorrectClauses maxVar clauseSize clausesSize varOffset = do
  cnf <- genCNF maxVar clauseSize clausesSize varOffset
  clauseOffset <- choose (1,maxBound)
  return $ cnf {
    getClauseNumb = clauseOffset + (getClauseNumb cnf)
  }

genCNFIncorrectVarNumb :: Word -> Word -> Word -> Word -> Gen CNF
genCNFIncorrectVarNumb maxVar clauseSize clausesSize varOffset = do
  cnf <- genCNF maxVar clauseSize clausesSize varOffset
  let actualMaxVar = findMaxVar $ getClauses cnf
  if actualMaxVar == 0 then
    return $ CNF 0 1 (clausesAddClause emptyClauses (
                clauseAddLiteral emptyClause (mkLiteral pos (mkVariable 1))
                )
             ) else do
    wrongVarNumb <- choose (0,actualMaxVar -1)
    return $ cnf {
      getMaxVar = wrongVarNumb
    }
    

instance Arbitrary CNF where
  arbitrary = genCNF 10 10 10 10
  shrink (CNF maxVar clSize clauses) =
    map (\cl -> CNF maxVar (getSizeClauses cl) cl) $ shrink clauses
