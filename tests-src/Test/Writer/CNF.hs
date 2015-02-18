module Test.Writer.CNF (
  tests
  ) where

import HSat.Writer.CNF
import HSat.Writer.CNF.Internal
import TestUtils
import HSat.Problem.BSP.CNF.Internal
import qualified Data.Vector as V
import HSat.Problem.BSP.Common
import Control.Monad (liftM)

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkCNFWriter" [
       mkCNFWriterTest1
       ],
    testGroup "addClauseComment" [
      addClauseCommentTest1,
      addClauseCommentTest2
      ],
    testGroup "addPreambleComment" [
      addPreambleCommentTest1
      ],
    testGroup "runCNFWriter" [
      runCNFWriterTest1
      ]
    ]

mkCNFWriterTest1 :: TestTree
mkCNFWriterTest1 =
  testProperty "mkCNFWriter returns correct values" $ property (
    \cnf ->
    let writer = mkCNFWriter cnf
        expectedClauses = V.map (\c -> WCL c []) . getVectClause . getClauses $ cnf
        gottenClauses = writeClauses writer
        expectedProblemLine = WPL (getMaxVar cnf) (getClauseNumb cnf) []
        gottenProblemLine = writeProblemLine writer
    in testEq "" expectedClauses gottenClauses .&&.
       testEq "" expectedProblemLine gottenProblemLine
       )

addClauseCommentTest1 :: TestTree
addClauseCommentTest1 =
  testProperty "addClauseComment (good int) == (Just cnfWriter)" $ forAll
  (
    do
      comment <- arbitrary
      writer <- arbitrary
      w <- case wplClauses . writeProblemLine $ writer of
        0 -> return Nothing
        n -> Just `liftM` choose (0,n-1)
      return (comment,writer,w)
      )
  (\(comment,writer,w) ->
    case w of
      Just index ->
        let writer' = addClauseComment index comment writer
        in case writer' of
          Just written ->
            let x = True
                y = False
            in property $ x===y
          Nothing -> property False
      Nothing -> property True
      )

addClauseCommentTest2 :: TestTree
addClauseCommentTest2 =
  testProperty "addClauseComment (bad int) == Nothing" $ forAll
  (
    do
      comment <- arbitrary
      writer <- arbitrary
      w <- choose (0,wplClauses . writeProblemLine $ writer)
      return (comment,writer,w)
      )
  (\(comment,writer,w) ->
    let writer' = addClauseComment w comment writer
    in case writer' of
          Nothing -> property True
          Just writer'' -> property False
          )

addPreambleCommentTest1 :: TestTree
addPreambleCommentTest1 =
  testProperty "addPreambleComment" $ property (
    \(comment,writer) ->
    let writer' = addPreambleComment comment writer
        expectedClauses = writeClauses writer
        gottenClauses = writeClauses writer'
        p = writeProblemLine writer
        expectedPreamble = WPL (wplVariables p) (wplClauses p) (
                               wplComments p ++ [comment])
        gottenPreamble = writeProblemLine writer'
    in testEq "" expectedClauses gottenClauses .&&.
       testEq "" expectedPreamble gottenPreamble
       )

runCNFWriterTest1 :: TestTree
runCNFWriterTest1 =
  testProperty "read . write == cnf" $ property (
    \cnfWriter ->
    let v' = wplVariables . writeProblemLine $ cnfWriter
        c' = wplClauses . writeProblemLine $ cnfWriter
        cl' = V.map wpClause . writeClauses $ cnfWriter
    in property (v' === v') .&&. (c' === c') .&&. (cl' === cl')
       )
