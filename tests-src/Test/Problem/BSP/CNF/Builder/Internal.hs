module Test.Problem.BSP.CNF.Builder.Internal (
  tests
  ) where

import TestUtils
import HSat.Problem.BSP.CNF.Builder.Internal
import TestUtils.Validate
import TestUtils.Problem.BSP.CNF.Builder

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    cnfBuilderTest1,
    cnfBuilderTest2,
    cnfBuilderError1,
    cnfBuilderError2,
    testGroup "canAddLiteral" [
       canAddLiteralTest1,
       canAddLiteralTest2
       ],
    testGroup "canFinalise" [
      canFinaliseTest1,
      canFinaliseTest2
      ],
    testGroup "canFinishClause" [
      canFinishClauseTest1,
      canFinishClauseTest2
      ]
    ]

cnfBuilderTest1 :: TestTree
cnfBuilderTest1 =
  testProperty "validate arbitrary == True" $ property testCNFBuilder
  where
    testCNFBuilder :: CNFBuilder -> Bool
    testCNFBuilder = validate

cnfBuilderTest2 :: TestTree
cnfBuilderTest2 =
  testProperty "validate badCNFBuilder == False" $
  forAll
  (genCNFInvalidBuilder 10 10 10 10)
  (not . validate)

cnfBuilderError1 :: TestTree
cnfBuilderError1 =
  testProperty "validate arbitrary CNFBuilderError == True" $
  property testCNFBuilderError
  where
    testCNFBuilderError :: CNFBuilderError -> Bool
    testCNFBuilderError = validate

cnfBuilderError2 :: TestTree
cnfBuilderError2 =
  testProperty "validate (bad CNFBuilderError) == False" $
  forAll
  (genCNFInvalidBuilder 10 10 10 10)
  (not . validate)

canAddLiteralTest1 :: TestTree
canAddLiteralTest1 =
  testProperty "canAddLiteral == True on valid CNFBuilder" $
  forAll
  (oneof [genCNFBuilderEmptyClause 10 10 10 10,
          genCNFBuilderLitInClause 10 10 10 10])
  canAddLiteral

canAddLiteralTest2 :: TestTree
canAddLiteralTest2 =
  testProperty "canAddLiteral == False on invalid CNFBuilder" $
  forAll
  (genCNFBuilderFinalise 10 10 10 10)
  (not . canAddLiteral)

canFinaliseTest1 :: TestTree
canFinaliseTest1 =
  testProperty "canFinalise == True on CNFBuilder on final clause" $
  forAll
  (genCNFBuilderFinalise 10 10 10 10)
  canFinalise

canFinaliseTest2 :: TestTree
canFinaliseTest2 =
  testProperty "canFinalise == False on non-final clause builder" $
  forAll
  (oneof [genCNFBuilderEmptyClause 10 10 10 10,
          genCNFBuilderLitInClause 10 10 10 10])
  (not . canFinalise)

canFinishClauseTest1 :: TestTree
canFinishClauseTest1 =
  testProperty "canFinishClause == True on valid CNFBuilder" $
  forAll
  (oneof [
      genCNFBuilderEmptyClause 10 10 10 10,
      genCNFBuilderLitInClause 10 10 10 10
      ])
  canFinishClause

canFinishClauseTest2 :: TestTree
canFinishClauseTest2 =
  testProperty "canFinishClause == False on invalid CNFBuilder" $
  forAll
  (genCNFBuilderFinalise 10 10 10 10)
  (not . canFinishClause)

