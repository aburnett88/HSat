{-|
Module      : Test.Problen.Instances.Common.Clauses.Internal
Description : The Clauses internal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the internal Clauses module
-}

module Test.Problem.Instances.Common.Clauses.Internal (
  tests     , -- :: TestTree
  genClauses  -- :: Word -> Int -> Gen Clauses
  ) where

import           Control.Applicative
import qualified Data.Vector                                    as V
import           HSat.Problem.Instances.Common.Clauses
import           HSat.Problem.Instances.Common.Clauses.Internal
import           Test.Problem.Instances.Common.Clause           (genClause)
import           TestUtils
import           TestUtils.Validate

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    clausesTest1,
    clausesTest2
    ]

clausesTest1 :: TestTree
clausesTest1 =
  testProperty ("validate arbitrary " `equiv` " True") $ property testClauses
  where
    testClauses :: Clauses -> Bool
    testClauses = validate

{-
Non valid Clauses are when the size is incorrect when comparing to the size of
the vector containing the Literals themselves
-}
clausesTest2 :: TestTree
clausesTest2 =
  testProperty ("validate (Clauses vect (sizeVect + choose (1,maxBound)) "
                `equiv` " False") $
  forAll
  (liftA2 (,) (V.fromList <$> arbitrary) (choose (1,maxBound)))
  (\(clauseList,wrongLen) ->
    let val = Clauses clauseList wrongLen
    in  not $ validate val
  )

{-
Checks that the length of the Clauses is consistent with the length of the
vector containing the Clause, then checks each element to confirm that it is
valid
-}
instance Validate Clauses where
  validate (Clauses clauseVector sizeClauses) =
    let actualSize = toEnum $ V.length clauseVector
    in (actualSize == sizeClauses) &&
       V.all validate clauseVector

genClauses             :: Word -> Int -> Gen Clauses
genClauses maxVar size =
  mkClauses <$> (flip V.replicateM (genClause maxVar size) =<< arbitrary)
                 
instance Arbitrary Clauses where
  arbitrary      = sized $ genClauses maxBound
  shrink clauses =
    map mkClauses $ shrink . getVectClause $ clauses
