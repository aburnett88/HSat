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
import           Data.Word
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.CNF.Builder.Internal
import           HSat.Problem.BSP.Common
import HSat.Problem.BSP.CNF.Internal
import           TestUtils
import qualified Test.Problem.BSP.CNF.Builder.Internal as Internal (tests)
import Test.Problem.BSP.CNF.Builder.Internal hiding (tests)
import Test.Problem.BSP.Common.Literal (genLiteral)
import TestUtils.Validate
import Test.Problem.BSP.CNF.Internal ()

name :: String
name = "Builder"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    testGroup "cnfBuilder" [
      cnfBuilderTest1,
      cnfBuilderTest2
      ],
    testGroup "cnfBuilder" [
      cnfBuilder'Test1,
      cnfBuilder'Test2
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
  testProperty "getExpdMaxVar = v, getExptdCl = c in cnfBuilder v c" $
  forAll
  (do
    vars <- choose (0,toInteger (maxBound :: Word))
    clauses <- choose (0,toInteger (maxBound :: Word))
    return (vars,clauses)
    )
  (\(vars,clauses) ->
    case cnfBuilder vars clauses of
      Left _ -> property False
      Right builder -> testCNFBuilderGenTest1 vars clauses builder
  )

genIntegerOutsideRange :: Gen Integer
genIntegerOutsideRange = oneof [
  liftM (\a -> if a==0 then -1 else if a < 0 then a else negate a) arbitrary,
  choose (1 + maxWord',(1+maxWord') ^ (2 :: Integer))
  ]

maxWord' :: Integer
maxWord' = toInteger (maxBound :: Word)

cnfBuilderTest2 :: TestTree
cnfBuilderTest2 =
  testProperty "cnfBuilder fails with correct error on incorrect initalisation" $
  forAll
  genVarsOrClausesOutsideRange
   (\(vars,clauses) ->
     case cnfBuilder vars clauses of
       Left (Initialisation v' c') ->
         v' < 0 .||. v' > maxWord' .||.
         c' < 0 .||. c' > maxWord'
       _ -> counterexample ("not failing " ++ show (cnfBuilder vars clauses)) $ property False
       )
   
testCNFBuilderGenTest1 :: Integer -> Integer -> CNFBuilder -> Property
testCNFBuilderGenTest1 vars cl builder =
  let valMaxVar = getExptdMaxVar builder
      valClNumb = getExptdClNumb builder
  in (valMaxVar === fromInteger vars) .&&.
     (valClNumb === fromInteger cl)

cnfBuilder'Test1 :: TestTree
cnfBuilder'Test1 =
  testProperty ("getExptdMaxVar = v, getExptdCl = c " ++
                "in cnfBuilder' v c") $ property
  (\(vars,clauses) ->
    testCNFBuilderGenTest1 vars clauses (cnfBuilder' vars clauses)
    )

genVarsOrClausesOutsideRange :: Gen (Integer,Integer)
genVarsOrClausesOutsideRange =  oneof [
      do
        vars <- choose (0,maxWord')
        clauses <- genIntegerOutsideRange
        return (vars,clauses),
      do
        vars <- genIntegerOutsideRange
        clauses <- choose (0,maxWord')
        return (vars,clauses),
      do
        vars <- genIntegerOutsideRange
        clauses <- genIntegerOutsideRange
        return (vars,clauses)
        ]

cnfBuilder'Test2 :: TestTree
cnfBuilder'Test2 =
  testProperty "cnfBuilder' vars or clauses outside range is correct" $
  forAll
  genVarsOrClausesOutsideRange
  (\(vars,clauses) ->
    validate (cnfBuilder' vars clauses)
    )

addLiteralTest1 :: TestTree
addLiteralTest1 =
  testProperty "addLiteral (var in range) gives correct values" $
  forAll
  (do
      cnf <- oneof [
        sized genCNFBuilderEmptyClause,
        sized genCNFBuilderLitInClause
        ]
      literal <- case getExptdMaxVar cnf of
        0 -> return Nothing
        i -> liftM (Just . literalToInteger) $ genLiteral i
      return (cnf,literal)
      )
  (\(cnfbuilder,literal') ->
    case literal' of
      Just literal ->
        case addLiteral literal cnfbuilder of
          Left _ -> property False
          Right builder -> addLiteralTest1Generic cnfbuilder builder literal
      Nothing -> property True
      )

testAddLiteral :: CNFBuilder -> Integer -> CNFBuilder
testAddLiteral builder lit =
  builder {
    getCurrClNumb = exptdSize,
    getCurrClause = exptdClause
    }
  where
    exptdSize = getCurrClNumb builder + (
      if clauseIsEmpty oldClause then
        1 else
        0
        )
    exptdClause = clauseAddLiteral oldClause (mkLiteralFromInteger lit)
    oldClause = getCurrClause builder

addLiteralTest1Generic :: CNFBuilder -> CNFBuilder -> Integer -> Property
addLiteralTest1Generic oldbuilder builder lit =
  let exptdBuilder = testAddLiteral oldbuilder lit
  in  exptdBuilder === builder

addLiteralTest2 :: TestTree
addLiteralTest2 =
  testProperty "addLiteral (var outof range) gives error" $
  forAll
    (do
      cnf <- oneof [
        sized genCNFBuilderEmptyClause,
        sized genCNFBuilderLitInClause
        ]
      x <- choose (getExptdMaxVar cnf,maxBound)
      literal <- liftM literalToInteger $ genLiteral x
      return (cnf,literal)
      )
  (\(cnfbuilder,literal) ->
    case addLiteral literal cnfbuilder of
      Left (VarOutsideRange gotten expected) ->
        let exptd = getExptdMaxVar cnfbuilder
            gotten' = abs literal
        in exptd === expected .&&.
           gotten' === gotten
      _ -> property False
      )

addLiteral'Test1 :: TestTree
addLiteral'Test1 =
  testProperty "addLiteral' (var in range) gives correct vlaue" $
  forAll
  (do
      cnf <- oneof [
        sized genCNFBuilderEmptyClause,
        sized genCNFBuilderLitInClause
        ]
      literal <- case getExptdMaxVar cnf of
        0 -> return Nothing
        _ -> liftM (Just . literalToInteger) $ genLiteral (getExptdMaxVar cnf)
      return (cnf,literal)
      )
  (\(builder,literal') ->
    case literal' of
      Nothing -> property True
      Just literal ->
        addLiteralTest1Generic builder (addLiteral' literal builder) literal
    )

addLiteral'Test2 :: TestTree
addLiteral'Test2 =
  testProperty "addLiteral' (var outof range) gives new correct value" $
  forAll
  (do
      cnf <- oneof [
        sized genCNFBuilderEmptyClause ,
        sized genCNFBuilderLitInClause 
        ]
      variable <- choose (getExptdMaxVar cnf+1,maxBound)
      sign <- arbitrary
      let literal = mkLiteral sign (mkVariable variable)
      return (cnf,literal)
      )
  (\(builder,literal) ->
    let literal' = literalToInteger literal
        expected = addLiteral' literal' builder
        gotten'  = testAddLiteral builder literal'
        gotten   = gotten' {
          getExptdMaxVar = getWord . getVariable $ literal
          }
    in gotten === expected
       )

finishClauseTest1 :: TestTree
finishClauseTest1 =
  testProperty "finishClause finishes the clause" $
  forAll
  (oneof [
      sized genCNFBuilderEmptyClause ,
      sized genCNFBuilderLitInClause 
      ])
  (\cnfbuilder ->
    case finishClause cnfbuilder of
      Left _ -> property False
      Right builder ->
        finishClauseTest1Generic builder cnfbuilder
        )

finishClauseTest1Generic :: CNFBuilder -> CNFBuilder -> Property
finishClauseTest1Generic builder oldbuilder =
  let currClause = getCurrClause oldbuilder
      exptdClauses = clausesAddClause (getCurrClauses oldbuilder) currClause
      gottenClauses = getCurrClauses builder
      exptdCount    = getCurrClNumb oldbuilder +
        if clauseIsEmpty currClause then
          1 else
          0
      gottenCount   = getCurrClNumb builder
  in clauseIsEmpty (getCurrClause builder) .&&.
     (exptdClauses === gottenClauses)        .&&.
     (exptdCount   === gottenCount)
  

finishClauseTest2 :: TestTree
finishClauseTest2 =
  testProperty "finishClasue on full CNF throws error" $
  forAll
  (sized genCNFBuilderFinalise )
  (\builder ->
    case finishClause builder of
      Left (IncorrectClauseNumber gotten expected) ->
        let expected' = getExptdClNumb builder
            gotten'    = expected'+1
        in counterexample "Expected: " (expected' === expected) .&&.
           counterexample "Gotten: " (gotten === gotten')
      _ -> property False
      )

finishClause'Test1 :: TestTree
finishClause'Test1 =
  testProperty "finishClause' has desired effects" $
  forAll
  (oneof [
      sized genCNFBuilderEmptyClause,
      sized genCNFBuilderLitInClause
      ])
  (\cnfbuilder ->
    finishClauseTest1Generic (finishClause' cnfbuilder) cnfbuilder)

finishClause'Test2 :: TestTree
finishClause'Test2 =
  testProperty "finishClause' pushes boundaries of problem if out of range" $
  forAll
  (sized genCNFBuilderFinalise )
  (\builder ->
    let builder' = finishClause' builder
        exptdClNumb = getCurrClNumb builder +
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
  (sized genCNFBuilderFinalise )
  (\builder ->
    case finalise builder of
      Left _ -> property False
      Right cnf -> genFinaliseTest1 builder cnf
      )

finaliseTest2 :: TestTree
finaliseTest2 =
  testProperty "finalise too early and error thrown" $
  forAll
  (oneof [
        sized genCNFBuilderEmptyClause,
        sized genCNFBuilderLitInClause
        ])
  (\builder ->
    case finalise builder of
      Left (IncorrectClauseNumber gotten expected) ->
        let gotten' = getCurrClNumb builder
            expected' = getExptdClNumb builder
        in (gotten' === gotten) .&&.
           (expected' === expected)
      _ -> property False
      )

finalise'Test1 :: TestTree
finalise'Test1 =
  testProperty "finalise on correct pure builder builds CNF" $
  forAll
  (sized genCNFBuilderFinalise)
  (\builder ->
    genFinaliseTest1 builder (finalise' builder)
    )

genFinaliseTest1 :: CNFBuilder -> CNF -> Property
genFinaliseTest1 builder cnf =
  let exptdMaxVar = getExptdMaxVar builder
      clNumb      = getCurrClNumb builder
      newClauses = if clauseIsEmpty (getCurrClause builder) then
                     getCurrClauses builder else
                     clausesAddClause
                     (getCurrClauses builder) (getCurrClause builder)
      cnf' = CNF exptdMaxVar clNumb newClauses
  in (cnf === cnf')

finalise'Test2 :: TestTree
finalise'Test2 =
  testProperty "finalise' on incorrect builder builds correc CNF" $
  forAll
  (oneof [
        sized genCNFBuilderEmptyClause,
        sized genCNFBuilderLitInClause
        ])
  (\builder ->
    let cnf' = finalise' builder
        currClNumb = getCurrClNumb builder
        exptdClNumb = getExptdClNumb builder
        maxVar = getExptdMaxVar builder
        newClauses = if clauseIsEmpty (getCurrClause builder) then
                       getCurrClauses builder else
                       clausesAddClause
                       (getCurrClauses builder) (getCurrClause builder)
        cnf = CNF maxVar currClNumb newClauses
    in (currClNumb < exptdClNumb) .&&.
       (cnf' === cnf) .&&.
       validate cnf'
       )

generalTest1 :: TestTree
generalTest1 =
  testProperty ("Run the Either methods on a list of Integer's and check " ++
                "the value recovered is what was expected") $ property
  (\cnf ->
    let (maxVar,clauseNumb,literals) = combined cnf
        exptdCNF   = evaluate (cnfBuilder maxVar clauseNumb) literals
    in  return cnf === exptdCNF
  )

generalTest'1 :: TestTree
generalTest'1 =
  testProperty ("Run the Pure methods on a list of Integer's and check " ++
                "the value recovered is what we expected")
  (\cnf ->
    let (maxVar,clauseNumb,literals) = combined cnf
        exptdCNF = evaluate' (cnfBuilder' maxVar clauseNumb) literals
    in cnf === exptdCNF
       )

combined :: CNF -> (Integer,Integer,[[Integer]])
combined cnf =
  let maxVar = toInteger $ getMaxVar cnf
      clauseNumb = toInteger $ getClauseNumb cnf
      literals = cnfToIntegers cnf
  in (maxVar,clauseNumb,literals)

evaluate :: CNFBuildErr -> [[Integer]] -> Either CNFBuilderError CNF
evaluate cnf [] = cnf >>= finalise
evaluate cnf (x:xs) = evaluate (evaluation cnf x) xs
  where
    evaluation :: CNFBuildErr -> [Integer] -> CNFBuildErr
    evaluation cnf' [] = cnf' >>= finishClause
    evaluation cnf' (y:ys) =
      evaluation (cnf' >>= addLiteral y) ys

evaluate' :: CNFBuilder -> [[Integer]] -> CNF
evaluate' cnf [] = finalise' cnf
evaluate' cnf (x:xs) = evaluate' (evaluation cnf x) xs
  where
    evaluation :: CNFBuilder -> [Integer] -> CNFBuilder
    evaluation cnf' [] = finishClause' cnf'
    evaluation cnf' (y:ys) =
      evaluation (addLiteral' y cnf') ys
  
  
        
