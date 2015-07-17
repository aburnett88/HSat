{-|
Module      : Test.Problen.Instances.Common.Clause.Internal
Description : The Clause internal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the Test Tree Node for the internal Clause module, as well as
associated Generator functions
-}

module Test.Problem.Instances.Common.Clause.Internal (
  tests    , -- TestTree
  genClause  -- :: Word -> Int -> Gen Clause
  ) where

import           Control.Applicative
import qualified Data.Vector                                   as V
import           HSat.Problem.Instances.Common.Clause
import           HSat.Problem.Instances.Common.Clause.Internal
import           Test.Problem.Instances.Common.Literal         (genLiteral)
import           TestUtils
import           TestUtils.Validate

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    clauseTest1,
    clauseTest2
    ]

clauseTest1 :: TestTree
clauseTest1 =
  testProperty ("validate arbitrary " `equiv` " True") $ property testClause
  where
    testClause :: Clause -> Bool
    testClause = validate

{-
Incorrect size should fail to validate
-}
clauseTest2 :: TestTree
clauseTest2 =
  testProperty ("validate (Clause vect (sizeVect + choose (1,maxBound)) "
                `equiv` " False") $
  forAll
  (liftA2 (,) (V.fromList <$> arbitrary) (choose (1,maxBound)))
  (\(litList,wrongLen) ->
    let val = Clause litList wrongLen
    in  not $ validate val
  )

genClauseFixedSize             :: Word -> Word -> Gen Clause
genClauseFixedSize _ 0         =
  let func = "genClauseFixedSize"
      file = "Test.Problem.Instances.Common.Clause.Internal"
      msg  = file ++ ":" ++ func
  in error (msg ++ " Second argument zero")
genClauseFixedSize size maxVar =
  mkClause <$> V.replicateM (fromEnum size) (genLiteral maxVar)

genClause          :: Word -> Int -> Gen Clause
genClause 0      _ = return $ mkClause V.empty
genClause maxVar size =
  flip genClauseFixedSize maxVar =<< (toEnum <$> choose (0,size))

{-
We pass the size parameter to determine the size of the Clause
-}
instance Arbitrary Clause where
  arbitrary = sized $ genClause maxBound
  shrink cl =
    map (mkClause . V.fromList) $ shrink . V.toList . getVectLiteral $ cl

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
