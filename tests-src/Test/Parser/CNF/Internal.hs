module Test.Parser.CNF.Internal (
  tests
  ) where

import TestUtils
import HSat.Parser.CNF.Internal
import Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Data.Word
import Data.Either
import HSat.Problem.BSP.Common
import qualified Data.Vector as V
import HSat.Problem.BSP.CNF.Builder.Internal
import Control.Monad (replicateM,liftM)
import Data.Text as T hiding (map,replicate)
import HSat.Problem.BSP.CNF.Builder

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
      parseClauseTest1
      ]
    ]

genComment :: Gen Text
genComment = do
  str <- arbitrary
  indent <- choose (0,100)
  return (pack (replicate indent ' ') `append` str)

parseCommentTest1 :: TestTree
parseCommentTest1 =
  testCase ("parseComment \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseComment (pack testStr) == (Right ())
    )
  where
    testStr = "c hello world"

parseCommentTest2 :: TestTree
parseCommentTest2 =
  testCase ("parseComment \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseComment (pack testStr) == (Right ())
    )
  where
    testStr = "   c hello world"

parseCommentTest3 :: TestTree
parseCommentTest3 =
  testCase ("parseComment \"" ++ testStr ++ "\"") $ assert (
    isLeft $ parseOnly parseComment (pack testStr)
    )
  where
    testStr = "c hello world\n"

parseCommentTest4 :: TestTree
parseCommentTest4 =
  testProperty "Parse random comments" $ forAll
  genComment
  (\text ->
    parseOnly parseComment text == (Right ())
    )

parseCommentsTest1 :: TestTree
parseCommentsTest1 =
  testCase ("parseComments \"" ++ testStr ++ "\"") $ assert (
    parseOnly parseComments (pack testStr) == (Right ())
    )
  where
    testStr = "c hello world\nc goodbye world"

parseCommentsTest2 :: TestTree
parseCommentsTest2 =
  testProperty ("parseComments randomly generated") $ forAll
  (do
      x <- choose (1,50)
      y <- replicateM x genComment
      return $ (T.unlines y)
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

parseProblemLineTest3 :: TestTree
parseProblemLineTest3 =
  testProperty "parseProblemLine parses correctly" $ forAll
  (do
      v <- arbitrary
      c <- arbitrary
      xs <- replicateM 5 (
        do
          x <- choose (0,10) :: Gen Int
          x <- choose (0,2) :: Gen Int
          Prelude.concat `liftM` replicateM x (
            do
              x <- choose (0,2) :: Gen Int
              return $ case x of
                0 -> ""
                1 -> "\t"
                2 -> " "
                )
            )
      return (xs,v,c)
      )
  (\(xs,v,c) ->
    let text = pack $ ((xs !! 0) ++ "p" ++ (xs !! 1) ++ "cnf" ++
                       (xs !! 2) ++ (show v) ++ (xs !! 3) ++ (show c) ++
                       (xs !! 4)
                       )
        cnf' = CNFBuilder v c 0 emptyClauses emptyClause
    in (parseOnly parseProblemLine text) === (Right . Right $ cnf')
       )
       
parseLiteralTest1 :: TestTree
parseLiteralTest1 =
  testCase ("parseLiteral \"" ++ testStr ++ "\"") $ assert (
    (parseOnly parseLiteral (pack testStr)) == (Right $ mkLiteralFromInteger word)
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
    word = (maxBound :: Word) + 100

parseLiteralTest5 :: TestTree
parseLiteralTest5 =
  testProperty ("parseLiteral sucessful") $ property (
    \word ->
    let res = parseOnly parseLiteral (pack $ show word)
    in case res of
      Left _ -> property $ False
      Right w -> w===word
      )

parseClauseTest1 :: TestTree
parseClauseTest1 =
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseOnly (parseClause $ return cnf) (pack testStr) == (Right . Right $ cnf')
    )
  where
    cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = V.foldl (flip addLiteral') cnf (getVectLiteral cl)
    cl = mkClauseFromLits $ map mkLiteralFromInteger [
      1,2,3,-4,-5,6,-7,8,9
                         ]
    testStr = "1 2 3 -4 -5 6 -7 8 9 0"


    
