module HSat.Writer.CNF (
  mkCNFWriter,
  addClauseComment,
  addPreambleComment,
  runCNFWriter
  ) where

import HSat.Writer.CNF.Internal
import HSat.Writer.Internal
import Data.Word
import Data.Text as T
import HSat.Problem.BSP.CNF.Internal
import qualified Data.Vector as V
import HSat.Problem.BSP.Common

mkCNFWriter :: CNF -> CNFWriter
mkCNFWriter (CNF v c cl) =
  let problem = WPL v c []
      clauses = V.map (flip WCL []) . getVectClause $ cl
  in CNFWriter problem clauses

addClauseComment :: Word -> Comment -> CNFWriter -> Maybe CNFWriter
addClauseComment w c writer
  | w < (wplClauses . writeProblemLine $ writer) =
    let (l,r) = V.splitAt (fromEnum w) $ writeClauses writer
        h' = WCL (wpClause (V.head r)) (wclComments (V.head r) ++ [c])
    in Just $ writer {
      writeClauses = l V.++ V.cons h' (V.tail r)
                     }
  | otherwise = Nothing

addPreambleComment :: Comment -> CNFWriter -> CNFWriter
addPreambleComment c writer =
  writer {
    writeProblemLine = (writeProblemLine writer) {
       wplComments = (wplComments . writeProblemLine $ writer) ++ [c]
                     }
                       }

runCNFWriter :: CNFWriter -> Text
runCNFWriter (CNFWriter p c) = T.unlines $ 
  runProblemLine p ++
  (Prelude.concat . V.toList . V.map runClauses $ c)
  where
    runProblemLine :: WrittenProblemLine -> [Text]
    runProblemLine (WPL v c comments) = ls ++ [m] ++ rs
      where
        (ls,rs) = runComment comments
        m = pack $ "p cnf " ++ show v ++ " " ++ show c
    runClauses :: WrittenClauseLine -> [Text]
    runClauses (WCL clause comments) = ls ++ [m] ++ rs
      where
        (ls,rs) = runComment comments
        m = runClauses' (getVectLiteral clause) empty


runClauses' :: V.Vector Literal -> Text -> Text
runClauses' vs t =
  if V.null vs then
    t `append` pack "0" else
    runClauses' (V.tail vs) (
      t `append` pack (
                  (show . literalToInteger . V.head $ vs) ++ " "
                  )
      )
                                         
