module TestUtils.Problem.BSP.CNF (
  genCNF,
  genCNFValid,
  genCNFInvalid
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
import Control.Monad (liftM3)
import Data.Word
import qualified Data.Vector as V

genCNF :: Gen Word -> Gen Word -> Gen Clauses -> Gen CNF
genCNF = liftM3 CNF

genCNFValid :: (Word,Word) -> (Word,Word) -> (Word,Word) -> Gen CNF
genCNFValid (_,0) _ _ = error ("genCNFValid (_,0)")
genCNFValid (0,_) _ _ = error "genCNFValid (0,_)"
genCNFValid varBound clauseBound clausesBound = do
  maxVar <- choose varBound
  noClauses <- choose clausesBound
  let litGen = genLiteral genSignValid (genVariableValidContext maxVar)
      clauseGen = genClauseValid clauseBound litGen
      constNoClauses = return noClauses
      clausesGen = genClauses (V.replicateM (fromEnum noClauses) clauseGen) constNoClauses
  genCNF (return maxVar) constNoClauses clausesGen

genCNFInvalid :: (Word,Word) -> (Word,Word) -> (Word,Word) -> Gen CNF
genCNFInvalid (_,0) _ _ = error "genCNFInvalid"
genCNFInvalid (0,_) _ _ = error "genCNFInvalid"
genCNFInvalid varBound clauseBound clausesBound = undefined

instance Arbitrary CNF where
  arbitrary = genCNFValid (1,100) (0,5) (0,5)
  shrink (CNF maxVar clSize clauses) =
    map (\cl -> CNF maxVar (getSizeClauses cl) cl) $ shrink clauses
