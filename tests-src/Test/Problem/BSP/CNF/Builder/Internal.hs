module Test.Problem.BSP.CNF.Builder.Internal (
  tests,
  genCNFBuilderEmptyClause,
  genCNFBuilderLitInClause,
  genCNFBuilderFinalise
  ) where

import TestUtils
import HSat.Problem.BSP.CNF.Builder.Internal
import TestUtils.Validate
import qualified Data.Vector as V
import HSat.Problem.BSP.Common
import Data.Word
import Control.Applicative
import Test.Problem.BSP.Common.Clauses (genClauses)
import Test.Problem.BSP.Common.Literal (genLiteral)
import Test.Problem.BSP.Common.Clause (genClause)

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    cnfBuilderTest1,
    cnfBuilderError1,
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

cnfBuilderError1 :: TestTree
cnfBuilderError1 =
  testProperty "validate arbitrary CNFBuilderError == True" $
  property testCNFBuilderError
  where
    testCNFBuilderError :: CNFBuilderError -> Bool
    testCNFBuilderError = validate

canAddLiteralTest1 :: TestTree
canAddLiteralTest1 =
  testProperty "canAddLiteral == True on valid CNFBuilder" $
  forAll
  (oneof [sized genCNFBuilderEmptyClause,
          sized genCNFBuilderLitInClause
          ]
   )
  canAddLiteral

canAddLiteralTest2 :: TestTree
canAddLiteralTest2 =
  testProperty "canAddLiteral == False on invalid CNFBuilder" $
  forAll
  (sized genCNFBuilderFinalise)
  (not . canAddLiteral)

canFinaliseTest1 :: TestTree
canFinaliseTest1 =
  testProperty "canFinalise == True on CNFBuilder on final clause" $
  forAll
  (sized genCNFBuilderFinalise)
  canFinalise

canFinaliseTest2 :: TestTree
canFinaliseTest2 =
  testProperty "canFinalise == False on non-final clause builder" $
  forAll
  (oneof [sized genCNFBuilderEmptyClause,
          sized genCNFBuilderLitInClause])
  (not . canFinalise)

canFinishClauseTest1 :: TestTree
canFinishClauseTest1 =
  testProperty "canFinishClause == True on valid CNFBuilder" $
  forAll
  (oneof [
      sized genCNFBuilderEmptyClause ,
      sized genCNFBuilderLitInClause 
      ])
  canFinishClause

canFinishClauseTest2 :: TestTree
canFinishClauseTest2 =
  testProperty "canFinishClause == False on invalid CNFBuilder" $
  forAll
  (sized genCNFBuilderFinalise)
  (not . canFinishClause)

instance Validate CNFBuilder where
  validate (CNFBuilder
            exptdMaxVar
            exptdClNumb
            currClNumb
            currClauses
            currClause) =
    let computedSize = sizeFunc currClauses currClause
    in (exptdClNumb >= currClNumb)                      &&
       V.all testVarInRange (getVectClause currClauses) &&
       (computedSize == currClNumb)                     &&
       testVarInRange currClause                        &&
       validate currClauses                             &&
       validate currClause
    where
      testVarInRange :: Clause -> Bool
      testVarInRange cl = V.all (varInRange exptdMaxVar) .
                          V.map getVariable $ getVectLiteral cl

instance Validate CNFBuilderError where
  validate (IncorrectClauseNumber gotten expected) =
    expected /= gotten
  validate (VarOutsideRange gotten expected)       =
    ((toInteger expected) < gotten) ||
    (gotten == 0)
  validate (Initialisation variables clauses)      =
    (variables < 0) || (clauses < 0) ||
    (variables > maxWord) || (clauses > maxWord)
    where
      maxWord = toInteger (maxBound :: Word)


genCNFBuilder      :: Int -> Gen CNFBuilder
genCNFBuilder size =
  oneof $ map (\f -> f size) [
    genCNFBuilderFinalise,
    genCNFBuilderEmptyClause,
    genCNFBuilderLitInClause
    ]

instance Arbitrary CNFBuilder where
  arbitrary = sized genCNFBuilder
  shrink (CNFBuilder
          maxVar
          setClNumb
          _
          currClauses
          currClause) =
    let mkBuilder = \(vect,clause) ->
          let size = sizeFunc vect clause
          in CNFBuilder maxVar setClNumb size vect clause
    in map mkBuilder $ shrink (currClauses,currClause)

sizeFunc      :: Clauses -> Clause -> Word
sizeFunc cl c = (+) (getSizeClauses cl) $ 
                if clauseIsEmpty c then 0 else 1
    
instance Arbitrary CNFBuilderError where
  arbitrary         = genCNFBuilderError
  shrink (IncorrectClauseNumber
          gotten
          expected) =
    filter validate . map (uncurry IncorrectClauseNumber) $
    shrink (gotten,expected)
  shrink (VarOutsideRange
          gotten
          expected) =
    filter validate . map (uncurry VarOutsideRange) $
    shrink (gotten,expected)
  shrink (Initialisation vars clauses) =
    filter validate . map (uncurry Initialisation) $
    shrink (vars,clauses)

genCNFBuilderFinalise :: Int -> Gen CNFBuilder
genCNFBuilderFinalise size = do
  maxVar'  <- toEnum `liftA` choose (1,size)
  clauses <- genClauses maxVar' size
  --Either return what we set as the maximum, or find the true maximum
  maxVar <- oneof [
    return maxVar',
    return $ findMaxVar clauses
    ]
  let sizeClauses = getSizeClauses clauses
      builder     =
        CNFBuilder maxVar sizeClauses sizeClauses clauses emptyClause
  return builder

{-
This function generates a triple which consists of:

A randomly generated Clauses type
A random Word that is strictly above the size of Clauses
A random Word that is strictly above the maximum Variable in Clauses
-}
genBuilderHelper      :: Int -> Gen (Clauses,Word,Word)
genBuilderHelper size = do
  maxVar'    <- toEnum `liftA` choose (1,size)
  clauses    <- genClauses maxVar' size
  targetSize <- ((+) (getSizeClauses clauses) . toEnum) `liftA` choose (1,size)
  maxVar     <- ((+) maxVar' . toEnum ) `liftA` choose (1,size)
  return (clauses,targetSize,maxVar)

{-
Generate a CNFBuilder with an empty Clauses. There should be at least a single
Clause left to add
-}
genCNFBuilderEmptyClause :: Int -> Gen CNFBuilder
genCNFBuilderEmptyClause size = do
  (clauses,targetSize,maxVar) <- genBuilderHelper size
  let builder =
        CNFBuilder maxVar targetSize
        (getSizeClauses clauses) clauses emptyClause
  return builder

{-
Generate a CNFBuilder. We should be on a clause, where we can still add literals
-}
genCNFBuilderLitInClause      :: Int -> Gen CNFBuilder
genCNFBuilderLitInClause size = do
  (clauses,targetSize,maxVar) <- genBuilderHelper size
  literal                     <- genLiteral maxVar
  clause                      <- flip clauseAddLiteral literal `liftA`
                                 genClause maxVar size
  let clauseSize = getSizeClauses clauses
      builder    = CNFBuilder maxVar (targetSize+1) clauseSize clauses clause
  return builder

genCNFBuilderError :: Gen CNFBuilderError
genCNFBuilderError =
  oneof [
    do
      expected <- choose (0,maxBound)
      gotten   <- choose (1,maxBound)
      let val  = IncorrectClauseNumber (expected+gotten) expected
      return val
    ,
    do
      expected <- choose (0,maxBound - 1)
      gotten   <- choose (expected + 1, maxBound)
      let val  = VarOutsideRange (toInteger gotten) expected
      return val
    ]

