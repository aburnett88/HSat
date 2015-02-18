{-|
Module      : Test.Problem.BSP.CNF.Builder
Description : The tests for the CNFBuilder test leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides tests for the CNFBuilder type
-}

module Test.Problem.BSP.CNF.Builder (
  tests
  ) where

import           Control.Monad (liftM)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.CNF.Builder.Internal
import           HSat.Problem.BSP.Common
import HSat.Problem.BSP.CNF.Internal
import           TestUtils
import qualified Test.Problem.BSP.CNF.Builder.Internal as Internal
import TestUtils.Problem.BSP.CNF.Builder
import TestUtils.Problem.BSP.Common.Literal
import HSat.Validate

name :: String
name = "Builder"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    testGroup "cnfBuilder" [
      cnfBuilderTest1
      ],
    testGroup "cnfBuilder" [
      cnfBuilder'Test1
      ],
    testGroup "addLiteral" [
      addLiteralTest1,
      addLiteralTest2
      ],
    testGroup "addLiteral'" [
      addLiteral'Test1,
      addLiteral'Test2
      ],
    testGroup "finishClause" [
      finishClauseTest1,
      finishClauseTest2
      ],
    testGroup "finishClause'" [
      finishClause'Test1,
      finishClause'Test2
      ],
    testGroup "finalise" [
      finaliseTest1,
      finaliseTest2
      ],
    testGroup "finalise'" [
      finalise'Test1,
      finalise'Test2
      ],
    testGroup "General Tests Either" [
      generalTest1
      ],
    testGroup "General Test Pure" [
      generalTest'1
      ]
    ]

cnfBuilderTest1 :: TestTree
cnfBuilderTest1 =
  testProperty "getExpdMaxVar = v, getExptdCl = c in cnfBuilder v c" $ property
  (\(vars,clauses) ->
    case cnfBuilder vars clauses of
      Left _ -> property $ False
      Right builder -> testCNFBuilderGenTest1 vars clauses builder
  )

testCNFBuilderGenTest1 :: Word -> Word -> CNFBuilder -> Property
testCNFBuilderGenTest1 vars cl builder =
  let valMaxVar = getExptdMaxVar builder
      valClNumb = getExptdClNumb builder
  in (valMaxVar === vars) .&&.
     (valClNumb === cl  )

cnfBuilder'Test1 :: TestTree
cnfBuilder'Test1 =
  testProperty ("getExptdMaxVar = v, getExptdCl = c " ++
                "in cnfBuilder' v c") $ property
  (\(vars,clauses) ->
    testCNFBuilderGenTest1 vars clauses (cnfBuilder' vars clauses)
    )

addLiteralTest1 :: TestTree
addLiteralTest1 =
  testProperty "addLiteral (var in range) gives correct values" $
  forAll
  (do
      cnf <- oneof [
        genCNFBuilderEmptyClause 10 10 10 10,
        genCNFBuilderLitInClause 10 10 10 10
        ]
      literal <- case getExptdMaxVar cnf of
        0 -> return Nothing
        i -> liftM Just $ genLiteral i
      return (cnf,literal)
      )
  (\(cnfbuilder,literal') ->
    case literal' of
      Just literal ->
        case (return cnfbuilder) >>= addLiteral literal of
          Left _ -> property $ False
          Right builder -> addLiteralTest1Generic cnfbuilder builder literal
      Nothing -> property $ True
      )

testAddLiteral :: CNFBuilder -> Literal -> CNFBuilder
testAddLiteral builder lit =
  builder {
    getCurrClNumb = exptdSize,
    getCurrClause = exptdClause
    }
  where
    exptdSize = (getCurrClNumb builder) + (
      if clauseIsEmpty oldClause then
        1 else
        0
        )
    exptdClause = clauseAddLiteral oldClause lit
    oldClause = getCurrClause builder

addLiteralTest1Generic :: CNFBuilder -> CNFBuilder -> Literal -> Property
addLiteralTest1Generic oldbuilder builder lit =
  let exptdBuilder = testAddLiteral oldbuilder lit
  in  exptdBuilder === builder

addLiteralTest2 :: TestTree
addLiteralTest2 =
  testProperty "addLiteral (var outof range) gives error" $
  forAll
    (do
      cnf <- oneof [
        genCNFBuilderEmptyClause 10 10 10 10,
        genCNFBuilderLitInClause 10 10 10 10
        ]
      x <- choose (getExptdMaxVar cnf,maxBound)
      literal <- genLiteral x
      return (cnf,literal)
      )
  (\(cnfbuilder,literal) ->
    case (return cnfbuilder) >>= addLiteral literal of
      Left (LitOutsideRange gotten expected) ->
        let exptd = getExptdMaxVar cnfbuilder
            gotten' = getWord . getVariable $ literal
        in exptd === expected .&&.
           gotten' === gotten
      _ -> property $ False
      )

addLiteral'Test1 :: TestTree
addLiteral'Test1 =
  testProperty "addLiteral' (var in range) gives correct vlaue" $
  forAll
  (do
      cnf <- oneof [
        genCNFBuilderEmptyClause 10 10 10 10,
        genCNFBuilderLitInClause 10 10 10 10
        ]
      literal <- case getExptdMaxVar cnf of
        0 -> return Nothing
        _ -> liftM Just $ genLiteral (getExptdMaxVar cnf)
      return (cnf,literal)
      )
  (\(builder,literal') ->
    case literal' of
      Nothing -> property $ True
      Just literal -> addLiteralTest1Generic builder (addLiteral' literal builder) literal
    )

addLiteral'Test2 :: TestTree
addLiteral'Test2 =
  testProperty "addLiteral' (var outof range) gives new correct value" $
  forAll
  (do
      cnf <- oneof [
        genCNFBuilderEmptyClause 10 10 10 10,
        genCNFBuilderLitInClause 10 10 10 10
        ]
      variable <- choose (getExptdMaxVar cnf+1,maxBound)
      sign <- arbitrary
      let literal = mkLiteral sign (mkVariable variable)
      return (cnf,literal)
      )
  (\(builder,literal) ->
    let expected = addLiteral' literal builder
        gotten'  = testAddLiteral builder literal
        gotten   = gotten' {
          getExptdMaxVar = (getWord . getVariable $ literal)
                           }
    in gotten === expected
       )

finishClauseTest1 :: TestTree
finishClauseTest1 =
  testProperty "finishClause finishes the clause" $
  forAll
  (oneof [
      genCNFBuilderEmptyClause 10 10 10 10,
      genCNFBuilderLitInClause 10 10 10 10
      ])
  (\cnfbuilder ->
    case (return cnfbuilder) >>= finishClause of
      Left _ -> property $ False
      Right builder ->
        finishClauseTest1Generic builder cnfbuilder
        )

finishClauseTest1Generic :: CNFBuilder -> CNFBuilder -> Property
finishClauseTest1Generic builder oldbuilder =
  let currClause = getCurrClause oldbuilder
      exptdClauses = clausesAddClause (getCurrClauses oldbuilder) currClause
      gottenClauses = getCurrClauses builder
      exptdCount    = (getCurrClNumb oldbuilder) +
        if (clauseIsEmpty currClause) then
          1 else
          0
      gottenCount   = getCurrClNumb builder
  in (clauseIsEmpty $ getCurrClause builder) .&&.
     (exptdClauses === gottenClauses)        .&&.
     (exptdCount   === gottenCount)
  

finishClauseTest2 :: TestTree
finishClauseTest2 =
  testProperty "finishClasue on full CNF throws error" $
  forAll
  (genCNFBuilderFinalise 10 10 10 10)
  (\builder ->
    case (return builder) >>= finishClause of
      Left (IncorrectClauseNumber gotten expected) ->
        let expected' = getExptdClNumb builder
            gotten'    = expected'+1
        in (counterexample "Expected: " (expected' === expected)) .&&.
           (counterexample "Gotten: " (gotten === gotten'))
      Right _ -> property $ False
      )

finishClause'Test1 :: TestTree
finishClause'Test1 =
  testProperty "finishClause' has desired effects" $
  forAll
  (oneof [
      genCNFBuilderEmptyClause 10 10 10 10,
      genCNFBuilderLitInClause 10 10 10 10
      ])
  (\cnfbuilder ->
    finishClauseTest1Generic (finishClause' cnfbuilder) cnfbuilder)

finishClause'Test2 :: TestTree
finishClause'Test2 =
  testProperty "finishClause' pushes boundaries of problem if out of range" $
  forAll
  (genCNFBuilderFinalise 10 10 10 10)
  (\builder ->
    let builder' = finishClause' builder
        exptdClNumb = (getCurrClNumb builder) +
                        if clauseIsEmpty (getCurrClause builder) then
                          1 else
                          0
        exptdSetClNumb = exptdClNumb
        gotCurrClNumb = getCurrClNumb builder'
        gotSetClNumb = getExptdClNumb builder'
    in (exptdClNumb === gotCurrClNumb) .&&.
       (exptdSetClNumb  === gotSetClNumb)
       )

finaliseTest1 :: TestTree
finaliseTest1 =
  testProperty "finalise on correct builder builds CNF" $
  forAll
  (genCNFBuilderFinalise 10 10 10 10)
  (\builder ->
    case finalise builder of
      Left _ -> property $ False
      Right cnf -> genFinaliseTest1 builder cnf
      )

finaliseTest2 :: TestTree
finaliseTest2 =
  testProperty "finalise too early and error thrown" $
  forAll
  (oneof [
      genCNFBuilderEmptyClause 10 10 10 10,
      genCNFBuilderLitInClause 10 10 10 10
      ])
  (\builder ->
    case finalise builder of
      Left (IncorrectClauseNumber gotten expected) ->
        let gotten' = getCurrClNumb builder
            expected' = getExptdClNumb builder
        in (gotten' === gotten) .&&.
           (expected' === expected)
      _ -> property $ False
      )

finalise'Test1 :: TestTree
finalise'Test1 =
  testProperty "finalise on correct pure builder builds CNF" $
  forAll
  (genCNFBuilderFinalise 10 10 10 10)
  (\builder ->
    genFinaliseTest1 builder (finalise' builder)
    )

genFinaliseTest1 :: CNFBuilder -> CNF -> Property
genFinaliseTest1 builder cnf =
  let exptdMaxVar = getExptdMaxVar builder
      clNumb      = getCurrClNumb builder
      newClauses = if clauseIsEmpty (getCurrClause builder) then
                     (getCurrClauses builder) else
                     clausesAddClause (getCurrClauses builder) (getCurrClause builder)
      cnf' = CNF exptdMaxVar clNumb newClauses
  in (cnf === cnf')

finalise'Test2 :: TestTree
finalise'Test2 =
  testProperty "finalise' on incorrect builder builds correc CNF" $
  forAll
  (oneof [
      genCNFBuilderEmptyClause 10 10 10 10,
      genCNFBuilderLitInClause 10 10 10 10
      ])
  (\builder ->
    let cnf' = finalise' builder
        currClNumb = getCurrClNumb builder
        exptdClNumb = getExptdClNumb builder
        maxVar = getExptdMaxVar builder
        newClauses = if clauseIsEmpty (getCurrClause builder) then
                       (getCurrClauses builder) else
                       clausesAddClause (getCurrClauses builder) (getCurrClause builder)
        cnf = CNF maxVar currClNumb newClauses
    in (currClNumb < exptdClNumb) .&&.
       (cnf' === cnf) .&&.
       (validate cnf')
       )

generalTest1 :: TestTree
generalTest1 =
  testProperty ("Run the Either methods on a list of Integer's and check " ++
                "the value recovered is what was expected") $ property
  (\cnf ->
    let maxVar     = getMaxVar cnf
        clauseNumb = getClauseNumb cnf
        literals   = map (map mkLiteralFromInteger) $ cnfToIntegers cnf
        exptdCNF   = evaluate (cnfBuilder maxVar clauseNumb) literals
    in  (return cnf) === exptdCNF
  )

generalTest'1 :: TestTree
generalTest'1 =
  testProperty ("Run the Pure methods on a list of Integer's and check " ++
                "the value recovered is what we expected")
  (\cnf ->
    let maxVar  = getMaxVar cnf
        clauseNumb = getClauseNumb cnf
        literals = map (map mkLiteralFromInteger) $ cnfToIntegers cnf
        exptdCNF = evaluate' (cnfBuilder' maxVar clauseNumb) literals
    in cnf === exptdCNF
       )

evaluate :: CNFBuildErr -> [[Literal]] -> Either CNFBuilderError CNF
evaluate cnf [] = cnf >>= finalise
evaluate cnf (x:xs) = evaluate (evaluation cnf x) xs
  where
    evaluation :: CNFBuildErr -> [Literal] -> CNFBuildErr
    evaluation cnf [] = cnf >>= finishClause
    evaluation cnf (y:ys) =
      evaluation (cnf >>= addLiteral y) ys

evaluate' :: CNFBuilder -> [[Literal]] -> CNF
evaluate' cnf [] = finalise' cnf
evaluate' cnf (x:xs) = evaluate' (evaluation cnf x) xs
  where
    evaluation :: CNFBuilder -> [Literal] -> CNFBuilder
    evaluation cnf [] = finishClause' cnf
    evaluation cnf (y:ys) =
      evaluation (addLiteral' y cnf) ys
  
  
        
