{-|
Module      : TestUtils.Problem.BSP.Common.Clause
Description : Generators for the Clause type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports Generator functions for the Clause type
-}

module TestUtils.Problem.BSP.Common.Clause (
  genClause,         -- :: Word -> Int -> Gen Clause
  genClauseFixedSize -- :: Word -> Word -> Gen Clause
  ) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector as V (replicateM,empty)
import           Data.Word
import           HSat.Problem.BSP.Common.Clause
import           HSat.Problem.BSP.Common.Clause.Internal
import           HSat.Problem.BSP.Common.Literal
import           TestUtils.Limits
import           TestUtils.Problem.BSP.Common.Literal
import           TestUtils.Test
import           TestUtils.Validate

{-
Doesn't work when a maximum variable of zero has been set. An error is thrown
-}
genClauseFixedSize             :: Word -> Word -> Gen Clause
genClauseFixedSize size 0      =
  let func = "genClauseFixedSize"
      file = "TestUtils.Problem.BSP.Common.Clause"
      msg  = file ++ ":" ++ func
  in error (msg ++ " Second argument zero")
genClauseFixedSize size maxVar = do
  vector <- V.replicateM (fromEnum size) $ genLiteral maxVar
  return $ mkClause vector

genClause          :: Word -> Int -> Gen Clause
genClause 0      _ = return $ mkClause V.empty
genClause maxVar size = do
  sizeOf <- choose (0,size)
  genClauseFixedSize (toEnum sizeOf) maxVar

{-
We pass the size parameter to determine the size of the Clause
-}
instance Arbitrary Clause where
  arbitrary = sized $ genClause maxBound
  shrink cl =
    map mkClause $ shrink . getVectLiteral $ cl

{-
Validation checks the size of the Clause is identical to the size of the
vector containing the elements. All the Literal's are also checked to make
sure that they are all valid
-}
instance Validate Clause where
  validate (Clause vect n) =
    let actualSize = toEnum $ V.length vect
    in (actualSize == n) &&
       V.all validate vect
