module Test.Parser.CNF.Internal (
  tests
  ) where

import           Control.Monad (replicateM,liftM)
import           Data.Attoparsec.Text (Parser)
import           Data.Attoparsec.Text as P
import           Data.Either
import           Data.Text as T hiding (map,replicate)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Parser.CNF.Internal
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.CNF.Builder.Internal
import           HSat.Problem.BSP.Common
import           TestUtils

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "parseComment" [
       parseCommentTest1,
       parseCommentTest2,
       parseCommentTest3,
       parseCommentTest4
       ],
    testGroup "parseComments" [
      parseCommentsTest1,
      parseCommentsTest2
      ],
    testGroup "parseProblemLine" [
      parseProblemLineTest1,
      parseProblemLineTest2,
      parseProblemLineTest3
      ],
    testGroup "parseLiteral" [
      parseLiteralTest1,
      parseLiteralTest2,
      parseLiteralTest3,
      parseLiteralTest4,
      parseLiteralTest5
      ],
    testGroup "parseClause" [
      parseClauseTest1,
      parseClauseTest2,
      parseClauseTest3,
      parseClauseTest4,
      parseClauseTest5
      ],
    testGroup "parseClauses" [
      parseClausesTest1,
      parseClausesTest2,
      parseClausesTest3
      ]
    ]

genComment :: Gen Text
genComment = do
  str <- arbitrary
  indent <- choose (0,100)
  return (pack (replicate indent ' ') `append` str)

parseCommentGen :: String -> TestTree
parseCommentGen str =
  testCase ("parseComment \"" ++ str ++ "\"") $ assert (
    parseOnly parseComment (pack str) == Right ()
    )

parseCommentTest1 :: TestTree
parseCommentTest1 =
  parseCommentGen "c hello world"

parseCommentTest2 :: TestTree
parseCommentTest2 =
  parseCommentGen "   c hello world"

parseCommentTest3 :: TestTree
parseCommentTest3 =
  testCase ("parseComment \"" ++ testStr ++ "\"") $ assert (
    isLeft $ parseOnly parseComment (pack testStr)
    )
  where
    testStr = "c hello world\n"

parseCommentTest4 :: TestTree
parseCommentTest4 =
  testProperty "Parse random comments" $
  forAll
  genComment
  (\text ->
    parseOnly parseComment text == Right ()
    )

parseCommentsTest1 :: TestTree
parseCommentsTest1 =
  testCase ("parseComments \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseComments (pack testStr) == Right ()
    )
  where
    testStr = "c hello world\nc goodbye world"

parseCommentsTest2 :: TestTree
parseCommentsTest2 =
  testProperty "parseComments randomly generated" $ forAll
  (do
      x <- choose (1,50)
      y <- replicateM x genComment
      return $ T.unlines y
      )
  (\text ->
    parseOnly parseComments text === Right ()
    )

parseProblemLineTest1 :: TestTree
parseProblemLineTest1 =
  testCase ("parseProblemLine \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseProblemLine (pack testStr) == (Right . Right $ cnf')
    )
  where
    testStr = "p cnf 24 2424"
    cnf' = CNFBuilder 24 2424 0 emptyClauses emptyClause

parseProblemLineTest2 :: TestTree
parseProblemLineTest2 =
  testCase ("parseProblemLine \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseProblemLine (pack testStr) == ( Right . Right $ cnf')
    )
  where
    testStr = "   p    cnf    24 \t2424"
    cnf' = CNFBuilder 24 2424 0 emptyClauses emptyClause

genSpace :: Gen String
genSpace = do
  number <- choose (1,10) :: Gen Int
  replicateM number $ do
    typ <- choose (0,1) :: Gen Int
    return $ case typ of
      0 -> ' '
      1 -> '\t'

parseProblemLineTest3 :: TestTree
parseProblemLineTest3 =
  testProperty "parseProblemLine parses correctly" $ forAll
  (do
      v <- arbitrary
      c <- arbitrary
      text <- do
        p <- return "p"
        spc1 <- genSpace
        cnf <- return "cnf"
        spc2 <- genSpace
        v <- return (show v)
        spc3 <- genSpace
        c <- return (show c)
        spc4 <- genSpace
        return $
          p   ++ spc1 ++
          cnf ++ spc2 ++
          v   ++ spc3 ++
          c   ++ spc4
      return (pack text,v,c)
      )
  (\(text,v,c) ->
    let cnf' = CNFBuilder v c 0 emptyClauses emptyClause
    in parseOnly parseProblemLine text === (Right . Right $ cnf')
       )
       
parseLiteralTest1 :: TestTree
parseLiteralTest1 =
  testCase ("parseLiteral \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseLiteral (pack testStr) ==
    (Right $ mkLiteralFromInteger word)
    )
  where
    testStr = show word
    word = 121

parseLiteralTest2 :: TestTree
parseLiteralTest2 =
  testCase ("parseLiteral \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseLiteral (pack testStr) == Right (mkLiteralFromInteger word)
    )
  where
    testStr = show word
    word = -121

parseLiteralTest3 :: TestTree
parseLiteralTest3 =
  testCase ("parseLiteral \"" ++ testStr ++ "\"") $ assert (
    isLeft $ parseOnly parseLiteral (pack testStr)
    )
  where
    testStr = show word
    word = 0

parseLiteralTest4 :: TestTree
parseLiteralTest4 =
  testCase ("parseLiteral \"" ++ testStr ++ "\"") $ assert (
    isLeft $ parseOnly parseLiteral (pack testStr)
    )
  where
    testStr = show word
    word = 100 + (toInteger (maxBound :: Word)):: Integer

parseLiteralTest5 :: TestTree
parseLiteralTest5 =
  testProperty "parseLiteral sucessful" $ property (
    \word ->
    let res = parseOnly parseLiteral (pack $ show word)
    in case res of
      Left _ -> property False
      Right w -> w===word
      )

parseClauseTest1 :: TestTree
parseClauseTest1 =
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseOnly (parseClause $ return cnf) (pack testStr) ==
    (Right . Right $ cnf')
    )
  where
    cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = V.foldl (flip addLiteral') cnf . V.map literalToInteger $ (getVectLiteral cl)
    cl = mkClauseFromLits $ map mkLiteralFromInteger [
      1,2,3,-4,-5,6,-7,8,9
                         ]
    testStr = "1 2 3 -4 -5 6 -7 8 9 0"
    

parseClauseTest2 :: TestTree
parseClauseTest2 =
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseOnly (parseClause $ return cnf) (pack testStr) ==
    (Right . Right $ cnf')
    )
    where
      cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
      cnf' = V.foldl (flip addLiteral') cnf . V.map literalToInteger $ (getVectLiteral cl)
      cl = mkClauseFromLits $ map mkLiteralFromInteger [
        1,2,3,-4,-5,6,-7,8,9
                           ]
      testStr = "1   2   3   -4 -5    6 -7   8            9 0"

{-
mkClauseStr :: [Integer] -> [String] -> String
mkClauseStr [] = " 0"
mkClauseStr (x:xs) = (show x) ++ " " ++ mkClauseStr xs
-}
parseClauseTest3 :: TestTree
parseClauseTest3 = 
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseOnly (parseClause $ return cnf) (pack testStr) ==
    (Right . Right $ cnf')
    )
    where
      cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
      cnf' = V.foldl (flip addLiteral') cnf . V.map literalToInteger $ (getVectLiteral cl)
      cl = mkClauseFromLits $ map mkLiteralFromInteger [
        1,2,3,-4,-5,6,-7,8,9
                           ]
      testStr = "1   2   3   -4 -5 \n   6 -7   8    \n        9 0"

parseClauseTest4 :: TestTree
parseClauseTest4 = 
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseOnly (parseClause $ return cnf) (pack testStr) ==
    (Right . Right $ cnf')
    )
    where
      cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
      cnf' = V.foldl (flip addLiteral') cnf . V.map literalToInteger $ (getVectLiteral cl)
      cl = mkClauseFromLits $ map mkLiteralFromInteger [
        1,2,3,-4,-5,6,-7,8,9
                           ]
      testStr = "1   2 \nc initial\n  3   -4 -5 \nc hello world\n   6 -7   8    \n        9 0"

--282

parseClauseTest5 :: TestTree
parseClauseTest5 =
  testProperty "parse randomly generated clauses" $
  forAll
  arbitrary
  (\bool -> bool === (not bool))

parseClausesTest1 :: TestTree
parseClausesTest1 =
  testCase ("parseClauses \"" ++ testStr ++ "\"") $ assert (
    True == False
    )
  where
    testStr = "UNWRITTEN YET"

parseClausesTest2 :: TestTree
parseClausesTest2 =
  testCase ("parseClauses \"" ++ testStr ++ "\"") $ assert (
    True == False
    )
  where
    testStr = "UNWRITTEN YET"

parseClausesTest3 :: TestTree
parseClausesTest3 =
  testProperty "parse randomly generated Clauses" $
  forAll
  arbitrary
  (\bool -> bool == (not bool))
