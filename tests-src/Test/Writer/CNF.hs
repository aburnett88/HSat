module Test.Writer.CNF (
  tests
  ) where

import HSat.Writer.CNF
import HSat.Writer.CNF.Internal
import TestUtils
import HSat.Problem.BSP.CNF.Internal
import qualified Data.Vector as V
import HSat.Problem.BSP.Common
import Control.Monad (liftM,replicateM,foldM)
import Data.Attoparsec.Text
import HSat.Parser.CNF
import Data.Maybe (fromJust,fromMaybe)
import Data.Word
import HSat.Writer.Internal
import Test.Problem.BSP.CNF ()

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    testGroup "getCNFFromWriter" [
       getCNFFromWriterTest1
       ],
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

getCNFFromWriterTest1 :: TestTree
getCNFFromWriterTest1 =
  testProperty "getCNFFromWriter returns correct values on hand made CNF" $ forAll
  (do
      cnf <- arbitrary
      preamblesToAdd <- choose (0,100)
      commentsToAdd <- choose (0,100)
      let w = mkCNFWriter cnf
      preambles <- replicateM preamblesToAdd arbitrary
      let p = foldl (\wr c -> addPreambleComment c wr) w preambles
      comments <-replicateM commentsToAdd arbitrary
      c <- foldM (\wr c -> 
                     case wplClauses . writeProblemLine $ wr of
                       0 -> return wr
                       n -> do
                         index <- choose (0,n-1)
                         return . fromJust $ addClauseComment index c wr)
           p comments
      return (cnf,c))
  (\(cnf,writer) ->
    let exptd = getCNFFromWriter writer
    in exptd === cnf
       )

mkCNFWriterTest1 :: TestTree
mkCNFWriterTest1 =
  testProperty "mkCNFWriter returns correct values" $ property (
    \cnf ->
    let writer = mkCNFWriter cnf
        expectedClauses = V.map (\c -> WCL c []) .
                          getVectClause . getClauses $ cnf
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
            let exptd = writer {
                  writeClauses = newClauses
                  }
                (l,r) = V.splitAt (fromEnum index) $ writeClauses writer
                newClauses = l V.++ (V.cons newCl (V.tail r))
                newCl = (V.head r) {
                  wpClause = (wpClause . V.head $ r),
                  wclComments = (wclComments . V.head $ r) ++ [comment]
                                }
            in counterexample
               ("Should have been equal")
               (exptd === written)
          Nothing ->
            counterexample
            "Should not have generated value outside range"
            False
      Nothing -> counterexample
                 "Cannot generate value"
                 True
      )

addClauseCommentTest2 :: TestTree
addClauseCommentTest2 =
  testProperty "addClauseComment (bad int) == Nothing" $ forAll
  (
    do
      comment <- arbitrary
      writer <- arbitrary
      w <- choose (wplClauses . writeProblemLine $ writer, maxBound)
      return (comment,writer,w)
      )
  (\(comment,writer,w) ->
    let writer' = addClauseComment w comment writer
    in counterexample
       ("Value excepted, should not have been")
       (writer' === Nothing)
       )

addPreambleCommentTest1 :: TestTree
addPreambleCommentTest1 =
  testProperty "addPreambleComment provides correct result" $ property (
    \(comment,writer) ->
    let preamble = writeProblemLine writer
        exptdPreamble = preamble {
          wplComments = (wplComments preamble) ++ [comment]
          }
        exptd = writer {
          writeProblemLine = exptdPreamble
          }
    in exptd === (addPreambleComment comment writer)
       )

runCNFWriterTest1 :: TestTree
runCNFWriterTest1 =
  testProperty "read . write == cnf" $ property $
  (\cnfWriter ->
    let exptd = return . return $ getCNFFromWriter cnfWriter
        gotten = parseOnly cnfParser (runCNFWriter cnfWriter)
    in gotten === exptd
       )

instance Arbitrary CNFWriter where
  arbitrary = do
    cnf <- arbitrary
    let writer = mkCNFWriter cnf
    noPreCommentsToAdd <- choose (0,100) :: Gen Int
    commentsPreamble <- replicateM noPreCommentsToAdd arbitrary
    noNormalCommentsToAdd <- choose (0,100) :: Gen Int
    commentsClauses <- replicateM noNormalCommentsToAdd (
      choose (0,getClauseNumb cnf))
    comments2 <- replicateM noNormalCommentsToAdd arbitrary
    let writer2 = foldl (flip addPreambleComment) writer commentsPreamble
        writer3 = foldl (\aaa (a,b) -> retain aaa a b) writer2 (
          zip commentsClauses comments2)
    return writer3
  shrink _ = []

instance Arbitrary WrittenClauseLine where
  arbitrary = do
    clause <- arbitrary
    comment <- arbitrary
    return $ WCL clause comment
  shrink (WCL c com) =
    map (WCL c) . shrink $ com

retain :: CNFWriter -> Word -> Comment -> CNFWriter
retain cnf w c =
  let cnf' = addClauseComment w c cnf
  in fromMaybe cnf cnf'
