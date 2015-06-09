module Test.Parser.CNF (
  tests
  ) where

import TestUtils
import qualified Test.Parser.CNF.Internal as Internal
import HSat.Problem.BSP.CNF.Builder.Internal
import HSat.Problem.BSP.CNF
import Data.Text.IO as T
import HSat.Parser.CNF
import Data.Attoparsec.Text (parseOnly)
import HSat.Writer.CNF
import Data.Either
import Test.Writer.CNF ()
import Test.Problem ()
import Control.Monad.Catch

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    testGroup "File Tests" fileTests,
    testGroup "Writer Tests" [
      testWriter1
      ]
    ]

fileTestGen :: FilePath -> Maybe (Either CNFBuilderError CNF) -> TestTree
fileTestGen fp val =
  testCase ("cnfParser file test " ++ fp) $ do
    fileContent <- T.readFile fp
    let result = parseOnly cnfParser fileContent
    case (val,result) of
      (Nothing,Left _) ->
        assertBool ("This should not have parsed: " ++ show result)
        (isLeft result)
      (Just (Left cnfErr),Right (Left gottenException)) ->
        case fromException gottenException of
          Just gottenException' -> assertBool "" $ gottenException' == cnfErr
          _ -> assertBool "Unexpected exception" False
      (Just (Right exptdCnf),Right (Right gottenCnf)) -> assertBool "" $ exptdCnf == gottenCnf
      _ -> assertBool "" False

prepare :: FilePath -> FilePath
prepare fp = "tests-src/Files/" ++ fp ++ ".cnf"

fileTests :: [TestTree]
fileTests = map (\(a,b) -> fileTestGen (prepare a) b) [
  ("test1-good", testCNF),
  ("test2-good", testCNF),
  ("test3-good", testCNF),
  ("test4-good", testCNF),
  ("test5-good", testCNF),
  ("test6-good", testCNF),
  ("test7-good", Nothing),
  ("test8-good", Just . Right . mkCNFFromIntegers $ []),
  ("test9-bad", Just . Left $ IncorrectClauseNumber 3 2),
  ("test10-bad", Just . Left $ IncorrectClauseNumber 1 2),
  ("test11-bad", Just . Left $ VarOutsideRange 6 3),
  ("test12-bad", Nothing),
  ("test13-bad", Just . Left $ Initialisation (-3) (-2)),
  --This test used to fail. But we updated our parser, it shoudl now pass
  ("test14-bad",testCNF)
  ]

testCNF :: Maybe (Either CNFBuilderError CNF)
testCNF = Just . Right . mkCNFFromIntegers $ [
  [1,-3],[2,3,-1]
         ]

testWriter1 :: TestTree
testWriter1 =
  testProperty "Write random CNF, then read and get back same CNF" $
  property
  (\writer ->
    let expectedCNF = getCNFFromWriter writer
        text        = runCNFWriter writer
        parseResult = parseOnly cnfParser text
    in case parseResult of
      Right (Right gottenCNF) -> gottenCNF === expectedCNF
      _ -> counterexample (show parseResult) False
    )
