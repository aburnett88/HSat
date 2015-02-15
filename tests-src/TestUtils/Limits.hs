module TestUtils.Limits (
  maxClauses,
  maxClause,
  maxVariable
  ) where

import Data.Word

maxClauses :: Int
maxClauses = 100

maxClause :: Int
maxClause = 100

maxVariable :: Word
maxVariable = maxBound

