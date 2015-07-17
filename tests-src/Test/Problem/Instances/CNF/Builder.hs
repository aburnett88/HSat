{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : Test.Problem.Instances.CNF.Builder
Description : The tests for the CNFBuilder test leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides tests for the CNFBuilder type
-}

module Test.Problem.Instances.CNF.Builder (
  tests -- TestTree
  ) where

import           Control.Applicative                         (liftA2)
import           Control.Monad.Catch
import           HSat.Problem.Instances.CNF
import           HSat.Problem.Instances.CNF.Builder
import           HSat.Problem.Instances.CNF.Builder.Internal
import           HSat.Problem.Instances.CNF.Internal
import           HSat.Problem.Instances.Common
import qualified Test.Problem.Instances.CNF.Builder.Internal as Internal (tests)
import           Test.Problem.Instances.CNF.Builder.Internal hiding (tests)
import           Test.Problem.Instances.CNF.Internal         ()
import           Test.Problem.Instances.Common.Literal       (genLiteral)
import           TestUtils
import           TestUtils.Validate

name :: String
name = "Builder"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    testGroup "cnfBuilder" [
      cnfBuilderTest1,
      cnfBuilderTest2,
      cnfBuilderTest3
      ],
    testGroup "cnfBuilder" [
      cnfBuilder'Test1,
      cnfBuilder'Test2,
      cnfBuilder'Test3
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

{-
================
cnfBuilder tests
================
-}

maxWord' :: Integer
maxWord' = toInteger (maxBound :: Word)

genMaxWord   :: Int -> Gen Integer
genMaxWord _ = choose (0,maxWord')

genCNFBuilderTest                    :: String -> String -> ((Integer,Integer) -> CNFBuilder -> Property) -> TestTree
genCNFBuilderTest initStr wrongStr f =
  testProperty (initStr ++ " in cnfBuilder v c") $
  forAll
  (liftA2 (,) (sized genMaxWord) (sized genMaxWord))
  (\pair@(vars,clauses) ->
    case cnfBuilder vars clauses of
     Right builder -> counterexample ("Incorrect " ++ wrongStr) (f pair builder)
     _                -> counterexample "Unexpected error" False
  )

cnfBuilderTest1 :: TestTree
cnfBuilderTest1 =
  genCNFBuilderTest ("getExptdMaxVar" `equiv` "v") "getExptdMaxVar"
  (\(vars,_) CNFBuilder{..} ->
    vars === toInteger getExptdMaxVar
  )

cnfBuilderTest2 :: TestTree
cnfBuilderTest2 =
  genCNFBuilderTest ("getExptdCl" `equiv` "c") "getExptdCl"
  (\(_,clauses) CNFBuilder{..} ->
    clauses === toInteger getExptdClNumb
  )

genIntegerOutsideRange      :: Int -> Gen Integer
genIntegerOutsideRange size = oneof [
  (\n -> case compare 0 n of
          EQ -> -1
          GT -> n
          LT -> negate n
          ) <$> arbitrary,
  choose (1+maxWord', (1+maxWord') ^ (toInteger size :: Integer))
  ]


cnfBuilderTest3 :: TestTree
cnfBuilderTest3 =
  testProperty ("cnfBuilder fails with correct error when incorrectly" ++
                "initialised") $ 
  forAll
  (sized genVarsOrClausesOutsideRange)
  (\(vars,clauses) ->
    case cnfBuilder vars clauses of
     Left exception -> case fromException exception of
       Just (Initialisation v' c') ->
         counterexample "Variables incorrect: " (v' < 0)        .||.
         counterexample "Variables incorrect: " (v' > maxWord') .||.
         counterexample "Clauses incorrect: "   (c' < 0)        .||.
         counterexample "Clauses incorrect: "   (c' > maxWord')
       _          -> counterexample "Unknown exception thrown" False
     _             -> counterexample "Unexpected CNF returned" False
  )

{-
==============
genCNFBuilder'
==============
-}

genCNFBuilder'Test                 :: String -> String ->
                                      (CNFBuilder -> (Integer,Integer) ->
                                       Property) -> TestTree
genCNFBuilder'Test title testStr f =
  testProperty (title ++ " in cnfBuilder' v c") $ property
  (\pair@(vars,clauses) -> counterexample ("Incorrect " ++ testStr)
                      (f (cnfBuilder' vars clauses) pair)
  )

cnfBuilder'Test1 :: TestTree
cnfBuilder'Test1 =
  genCNFBuilder'Test ("getExptdMaxVar" `equiv` "v") "getExptdMaxVar"
  (\builder (vars,_) ->
    getExptdMaxVar builder === fromInteger vars
  )

cnfBuilder'Test2 :: TestTree
cnfBuilder'Test2 =
  genCNFBuilder'Test ("getClNumb" `equiv` "c") "getClNumb"
  (\builder (_,clauses) ->
    getExptdClNumb builder === fromInteger clauses
  )
    
genVarsOrClausesOutsideRange      :: Int -> Gen (Integer,Integer)
genVarsOrClausesOutsideRange size =  oneof [
  liftA2 (,) (genMaxWord size) (genIntegerOutsideRange size)            ,
  liftA2 (,) (genIntegerOutsideRange size) (genMaxWord size)            ,
  liftA2 (,) (genIntegerOutsideRange size) (genIntegerOutsideRange size)
  ]

cnfBuilder'Test3 :: TestTree
cnfBuilder'Test3 =
  testProperty "cnfBuilder' vars or clauses outside range is correct" $
  forAll
  (sized genVarsOrClausesOutsideRange)
  (\(vars,clauses) ->
    validate (cnfBuilder' vars clauses)
  )

{-
==========
addliteral
==========
-}

genCNFBuilderToAdd      :: Int -> Gen CNFBuilder
genCNFBuilderToAdd size = oneof [
  genCNFBuilderEmptyClause size,
  genCNFBuilderLitInClause size
  ]

addLiteralTest1 :: TestTree
addLiteralTest1 =
  testProperty "addLiteral (var in range) gives correct values" $
  forAll
  (sized genBuilderGoodLiteral)
  (\(builder,literal) ->
    case addLiteral literal builder of
     Left _         -> counterexample "Unexpected error thrown" False
     Right builder' -> addLiteralTestGeneric builder builder' literal
  )

genBuilderGoodLiteral      :: Int -> Gen (CNFBuilder, Integer)
genBuilderGoodLiteral size = do
  builder <- genCNFBuilderToAdd size
  literal <- literalToInteger <$> genLiteral (getExptdMaxVar builder)
  return (builder,literal)

addLiteralTestGeneric                        :: CNFBuilder -> CNFBuilder -> Integer -> Property
addLiteralTestGeneric oldbuilder builder lit =
  let exptdBuilder = testAddLiteral oldbuilder (mkLiteralFromInteger lit)
  in  exptdBuilder === builder

testAddLiteral                                   :: CNFBuilder -> Literal -> CNFBuilder
testAddLiteral newBuilder@CNFBuilder{..} literal =
  newBuilder {
    getCurrClNumb = getCurrClNumb +
                    if clauseIsEmpty getCurrClause then 1 else 0,
    getCurrClause = clauseAddLiteral getCurrClause literal
    }

genBuilderBadLiteral      :: Int -> Gen (CNFBuilder,Integer)
genBuilderBadLiteral size = do
  cnf <- genCNFBuilderToAdd size
  literal <- literalToInteger <$> (genLiteral =<< choose (getExptdMaxVar cnf,maxBound))
  return (cnf,literal)

addLiteralTest2 :: TestTree
addLiteralTest2 =
  testProperty "addLiteral (var outof range) gives error" $
  forAll
  (sized genBuilderBadLiteral)
  (\(cnfbuilder,literal) ->
    case addLiteral literal cnfbuilder of
      Left exception -> case fromException exception of
        Just (VarOutsideRange gotten expected) ->
          let exptd'  = getExptdMaxVar cnfbuilder
              gotten' = abs literal
          in counterexample "Gotten: "   (gotten === gotten' ) .&&.
             counterexample "Expected: " (expected === exptd')
        _ -> counterexample "Unknown exception thrown" False
      _ -> counterexample "Unexpected CNF returned" False
  )

addLiteral'Test1 :: TestTree
addLiteral'Test1 =
  testProperty "addLiteral' (var in range) gives correct value" $
  forAll
  (sized genBuilderGoodLiteral)
  (\(builder,literal) ->
    addLiteralTestGeneric builder (addLiteral' literal builder) literal
  )

addLiteral'Test2 :: TestTree
addLiteral'Test2 =
  testProperty "addLiteral' (var outof range) gives new correct value" $
  forAll
  (sized genBuilderBadLiteral)
  (\(builder,literal) ->
    let literal' = mkLiteralFromInteger literal
        expected = addLiteral' literal builder
        gotten'  = testAddLiteral builder literal'
        gotten   = gotten' {
          getExptdMaxVar = getWord . getVariable $ literal'
          }
    in gotten === expected
       )

{-
==================
finishClause tests
==================
-}

finishClauseTest1 :: TestTree
finishClauseTest1 =
  testProperty "finishClause finishes the clause" $
  forAll
  (sized genCNFBuilderToAdd)
  (\cnfbuilder ->
    case finishClause cnfbuilder of
      Left _ -> counterexample "Unexpected exception" False
      Right builder ->
        finishClauseTest1Generic builder cnfbuilder
  )

{-
Tests that the builder is what is expected based off the old builder
-}
finishClauseTest1Generic                    :: CNFBuilder -> CNFBuilder -> Property
finishClauseTest1Generic builder oldbuilder =
  let oldCurrClause  = getCurrClause oldbuilder
      oldCurrClauses = getCurrClauses oldbuilder
      exptdClauses   = clausesAddClause oldCurrClauses oldCurrClause
      gottenClauses  = getCurrClauses builder
      exptdCount     = getCurrClNumb oldbuilder +
                       if clauseIsEmpty oldCurrClause then
                         1 else 0
      gottenCount   = getCurrClNumb builder
  in clauseIsEmpty (getCurrClause builder)   .&&.
     (exptdClauses === gottenClauses)        .&&.
     counterexample "Current clause numbers not correct" (exptdCount === gottenCount)
  

finishClauseTest2 :: TestTree
finishClauseTest2 =
  testProperty "finishClause on full CNF throws error" $
  forAll
  (sized genCNFBuilderFinalise)
  (\builder ->
    case finishClause builder of
    Left exception ->
      case fromException exception of
        Just (IncorrectClauseNumber gotten expected) ->
          let expected' = getExptdClNumb builder
              gotten'   = expected' + 1
          in counterexample "Expected: " (expected === expected) .&&.
             counterexample "Gotten: "      (gotten === gotten')
        _ -> counterexample "Unknown exception thrown" False
    _ -> counterexample "Unexpected CNF returned" False
  )

finishClause'Test1 :: TestTree
finishClause'Test1 =
  testProperty "finishClause' has desired effects" $
  forAll
  (sized genCNFBuilderToAdd)
  (\cnfbuilder ->
    finishClauseTest1Generic (finishClause' cnfbuilder) cnfbuilder)

finishClause'Test2 :: TestTree
finishClause'Test2 =
  testProperty "finishClause' pushes boundaries of problem if out of range" $
  forAll
  (sized genCNFBuilderFinalise )
  (\builder ->
    let builder'      = finishClause' builder
        exptdClNumb   = getCurrClNumb builder +
                        if clauseIsEmpty (getCurrClause builder) then
                          1 else
                          0
        exptdSetClNumb = exptdClNumb
        gotCurrClNumb  = getCurrClNumb builder'
        gotSetClNumb   = getExptdClNumb builder'
    in (exptdClNumb     === gotCurrClNumb) .&&.
       (exptdSetClNumb  === gotSetClNumb)
       )

{-
==============
finalise tests
==============
-}

finaliseTest1 :: TestTree
finaliseTest1 =
  testProperty "finalise on correct builder builds CNF" $
  forAll
  (sized genCNFBuilderFinalise )
  (\builder ->
    case finalise builder of
      Left _ -> counterexample "Unexpected exception" False
      Right cnf -> genFinaliseTest1 builder cnf
      )

finaliseTest2 :: TestTree
finaliseTest2 =
  testProperty "finalise too early and error thrown" $
  forAll
  (sized genCNFBuilderToAdd)
  (\builder@CNFBuilder{..} ->
    case finalise builder of
      Left exception ->
        case fromException exception of
          Just (IncorrectClauseNumber gotten expected) ->
            let gotten' = getCurrClNumb 
                expected' = getExptdClNumb
            in (gotten' === gotten) .&&.
               (expected' === expected)
          _ -> counterexample "Unknown exception thrown" False
      Right _ -> counterexample "Unexpected CNF returned" False
  )

finalise'Test1 :: TestTree
finalise'Test1 =
  testProperty "finalise on correct pure builder builds CNF" $
  forAll
  (sized genCNFBuilderFinalise)
  (\builder ->
    genFinaliseTest1 builder (finalise' builder)
    )

genFinaliseTest1             :: CNFBuilder -> CNF -> Property
genFinaliseTest1 builder cnf =
  let (test,_,_,_) = genFinaliseTest1' builder cnf
  in test

genFinaliseTest1'                    :: CNFBuilder -> CNF ->
                                        (Property,Word,Word,CNF)
genFinaliseTest1' CNFBuilder{..} cnf =
  let newClauses = if clauseIsEmpty getCurrClause then getCurrClauses else
                     clausesAddClause getCurrClauses getCurrClause
      cnf'       = CNF getExptdMaxVar getCurrClNumb newClauses
  in (cnf === cnf', getCurrClNumb, getExptdClNumb,cnf')

finalise'Test2 :: TestTree
finalise'Test2 =
  testProperty "finalise' on incorrect builder builds correct CNF" $
  forAll
  (sized genCNFBuilderToAdd)
  (\builder ->
    let (test,currClNumb,exptdClNumb,cnf') =
          genFinaliseTest1' builder (finalise' builder)
    in test          .&&.
       validate cnf' .&&.
       (currClNumb < exptdClNumb)
  )

{-
=============
general Tests
=============
-}

generalTest1 :: TestTree
generalTest1 =
  testProperty ("Run the Either methods on a list of Integer's and check " ++
                "the value recovered is what was expected") $ property
  (\cnf ->
    let (maxVar,clauseNumb,literals) = combined cnf
        exptdCNF                     =
          evaluate (cnfBuilder maxVar clauseNumb) literals
          :: Either SomeException CNF
    in case exptdCNF of
        Right cnf' -> counterexample "Unequal CNF's: " $ cnf === cnf'
        _ -> counterexample "Unknown exception thrown: " False
  )

generalTest'1 :: TestTree
generalTest'1 =
  testProperty ("Run the Pure methods on a list of Integer's and check " ++
                "the value recovered is what we expected")
  (\cnf ->
    let (maxVar,clauseNumb,literals) = combined cnf
        exptdCNF                     =
          evaluate' (cnfBuilder' maxVar clauseNumb) literals
    in cnf === exptdCNF
  )

combined     :: CNF -> (Integer,Integer,[[Integer]])
combined cnf =
  let maxVar     = toInteger $ getMaxVar cnf
      clauseNumb = toInteger $ getClauseNumb cnf
      literals   = cnfToIntegers cnf
  in (maxVar,clauseNumb,literals)

evaluate            :: (MonadThrow m) => m CNFBuilder -> [[Integer]] -> m CNF
evaluate cnf []     = cnf >>= finalise
evaluate cnf (x:xs) = evaluate (evaluation cnf x) xs
  where
    evaluation cnf' []     = cnf' >>= finishClause
    evaluation cnf' (y:ys) =
      evaluation (cnf' >>= addLiteral y) ys

evaluate'            :: CNFBuilder -> [[Integer]] -> CNF
evaluate' cnf []     = finalise' cnf
evaluate' cnf (x:xs) = evaluate' (evaluation cnf x) xs
  where
    evaluation             :: CNFBuilder -> [Integer] -> CNFBuilder
    evaluation cnf' []     = finishClause' cnf'
    evaluation cnf' (y:ys) =
      evaluation (addLiteral' y cnf') ys
