{-|
Module      : TestUtils.Problem.BSP.Common.Clauses
Description : Generators for the Clauses type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports Generator functions for the Clauses type
-}

module TestUtils.Problem.BSP.Common.Clauses (
  genClauses -- :: Word -> Int -> Gen Clauses
  ) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Problem.BSP.Common.Clause
import           HSat.Problem.BSP.Common.Clauses
import           HSat.Problem.BSP.Common.Clauses.Internal
import           TestUtils.Limits
import           TestUtils.Problem.BSP.Common.Clause
import           TestUtils.Test
import           TestUtils.Validate

genClauses :: Word -> Int -> Gen Clauses
genClauses maxVar size = do
  sizeOf <- choose (0,size)
  vector <- V.replicateM (fromEnum sizeOf) $ genClause maxVar size
  return $ mkClauses vector

instance Arbitrary Clauses where
  arbitrary      = sized $ genClauses maxBound
  shrink clauses =
    map mkClauses $ shrink . getVectClause $ clauses

{-
Checks that the length of the Clauses is consistent with the length of the vector containing the Clause, then checks each element to confirm that it is valid
-}
instance Validate Clauses where
  validate (Clauses vector sizeClauses) =
    let actualSize = toEnum $ V.length vector
    in (actualSize == sizeClauses) &&
       V.all validate vector
