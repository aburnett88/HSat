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
    case val of
      Nothing -> assertBool "This should not have been parsed"  (isLeft result)
      Just exptdVal -> case result of
        Right cnf' -> exptdVal @=? cnf'
        _ -> assertBool "Parser failed when it should not have done so" False

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
  ("test14-bad", Nothing)
  ]

testCNF :: Maybe (Either CNFBuilderError CNF)
testCNF = Just . Right . mkCNFFromIntegers $ [
  [1,-3],[2,3,-1]
         ]

testWriter1 :: TestTree
testWriter1 =
  testProperty "Write random CNF, then read and get back same CNF" $
  forAll
  (do
      writer <- arbitrary
      let cnf = getCNFFromWriter writer
      return (writer,cnf)
      )
  (\(writer,cnf) ->
    let text = runCNFWriter writer
        cnf' = parseOnly cnfParser text
    in case cnf' of
      Right (Right newCnf) -> cnf === newCnf
      _ -> property False
  )
