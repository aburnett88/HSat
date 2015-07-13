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

import Control.Applicative (liftA2)
import           Control.Monad                               (liftM)
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

genMaxWord :: Int -> Gen Integer
genMaxWord _ = choose (0,maxWord')

genCNFBuilderTest :: String -> String -> ((Integer,Integer) -> CNFBuilder -> Property) -> TestTree
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
    vars === (toInteger getExptdMaxVar)
  )

cnfBuilderTest2 :: TestTree
cnfBuilderTest2 =
  genCNFBuilderTest ("getExptdCl" `equiv` "c") "getExptdCl"
  (\(_,clauses) CNFBuilder{..} ->
    clauses === (toInteger getExptdClNumb)
  )
  
genIntegerOutsideRange :: Gen Integer
genIntegerOutsideRange = oneof [
  (\n -> case compare 0 n of
          EQ -> -1
          GT -> n
          LT -> negate n
          ) <$> arbitrary,
  choose (1+maxWord', (1+maxWord') ^ (2 :: Integer))
  ]

maxWord' :: Integer
maxWord' = toInteger (maxBound :: Word)

cnfBuilderTest3 :: TestTree
cnfBuilderTest3 =
  testProperty ("cnfBuilder fails with correct error when incorrectly" ++
                "initialised") $ 
  forAll
  genVarsOrClausesOutsideRange
   (\(vars,clauses) ->
     case cnfBuilder vars clauses of
       Left exception -> case fromException exception of
         Just (Initialisation v' c') ->
           (counterexample "Variables incorrect: " $ v' < 0) .||.
           (counterexample "Variables incorrect: " $ v' > maxWord') .||.
           (counterexample "Clauses incorrect: " $ c' < 0) .||.
           (counterexample "Clauses incorrect: " $ c' > maxWord')
         _ -> counterexample "Unknown exception thrown" False
       _ -> counterexample "Unexpected CNF returned" False
   )

{-
==============
genCNFBuilder'
==============
-}

genCNFBuilder'Test :: String -> String ->
                      (CNFBuilder -> (Integer,Integer) -> Property) -> TestTree
genCNFBuilder'Test title testStr f =
  testProperty (title ++ " in cnfBuilder' v c") $ property
  (\pair@(vars,clauses) -> counterexample ("Incorrect " ++ testStr)
                      (f (cnfBuilder' vars clauses) pair)
  )

cnfBuilder'Test1 :: TestTree
cnfBuilder'Test1 =
  genCNFBuilder'Test ("getExptdMaxVar" `equiv` "v") "getExptdMaxVar"
  (\builder (vars,_) ->
    getExptdMaxVar builder === (fromInteger vars)
  )

cnfBuilder'Test2 :: TestTree
cnfBuilder'Test2 =
  genCNFBuilder'Test ("getClNumb" `equiv` "c") "getClNumb"
  (\builder (_,clauses) ->
    getExptdClNumb builder === (fromInteger clauses)
  )
    
genVarsOrClausesOutsideRange :: Gen (Integer,Integer)
genVarsOrClausesOutsideRange =  oneof [
  liftA2 (,) (sized genMaxWord) genIntegerOutsideRange            ,
  liftA2 (,) genIntegerOutsideRange (sized genMaxWord)            ,
  liftA2 (,) genIntegerOutsideRange genIntegerOutsideRange
  ]

cnfBuilder'Test3 :: TestTree
cnfBuilder'Test3 =
  testProperty "cnfBuilder' vars or clauses outside range is correct" $
  forAll
  genVarsOrClausesOutsideRange
  (\(vars,clauses) ->
    validate (cnfBuilder' vars clauses)
    )

{-
==========
addliteral
==========
-}

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
        i -> (Just . literalToInteger) <$> genLiteral i
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
      Left exception -> case fromException exception of
        Just (VarOutsideRange gotten expected) ->
          let exptd'  = getExptdMaxVar cnfbuilder
              gotten' = abs literal
          in (counterexample "Gotten: " $ gotten === gotten') .&&.
             (counterexample "Expected: " $ expected === exptd')
        _ -> counterexample "Unknown exception thrown" False
      _ -> counterexample "Unexepcted CNF returned" False
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
     (counterexample (show builder ++ " " ++ show oldbuilder) (exptdCount   === gottenCount))
  

finishClauseTest2 :: TestTree
finishClauseTest2 =
  testProperty "finishClasue on full CNF throws error" $
  forAll
  (sized genCNFBuilderFinalise )
  (\builder ->
    case finishClause builder of
    Left exception ->
      case fromException exception of
        Just (IncorrectClauseNumber gotten expected) ->
          let expected' = getExptdClNumb builder
              gotten'   = expected' + 1
          in counterexample "Expected: " (expected === expected) .&&.
             counterexample "Gotten: " (gotten === gotten')
        _ -> counterexample "Unknown exception thrown" False
    _ -> counterexample "Unexpected CNF returned" False
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
      Left exception ->
        case fromException exception of
          Just (IncorrectClauseNumber gotten expected) ->
            let gotten' = getCurrClNumb builder
                expected' = getExptdClNumb builder
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
        exptdCNF   = (evaluate (cnfBuilder maxVar clauseNumb) literals) :: Either SomeException CNF
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
        exptdCNF = evaluate' (cnfBuilder' maxVar clauseNumb) literals
    in cnf === exptdCNF
       )

combined :: CNF -> (Integer,Integer,[[Integer]])
combined cnf =
  let maxVar = toInteger $ getMaxVar cnf
      clauseNumb = toInteger $ getClauseNumb cnf
      literals = cnfToIntegers cnf
  in (maxVar,clauseNumb,literals)

evaluate :: (MonadThrow m) => m CNFBuilder -> [[Integer]] -> m CNF
evaluate cnf [] = cnf >>= finalise
evaluate cnf (x:xs) = evaluate (evaluation cnf x) xs
  where
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
