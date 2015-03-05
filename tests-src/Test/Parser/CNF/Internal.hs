module Test.Parser.CNF.Internal (
  tests
  ) where

import           Control.Monad (replicateM,liftM)
import           Data.Attoparsec.Text (Parser)
import           Data.Attoparsec.Text as P hiding (parseTest)
import           Data.Either
import           Data.Text as T hiding (map,replicate)
import qualified Data.Vector as V
import           HSat.Parser.CNF.Internal
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.CNF.Builder.Internal
import           TestUtils
import Control.Applicative
import HSat.Problem.BSP.Common
import TestUtils.Problem.BSP.CNF.Builder
import Data.Monoid

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
    testGroup "parseNonZeroInteger" [
      parseNonZeroIntegerTest1,
      parseNonZeroIntegerTest2,
      parseNonZeroIntegerTest3,
      parseNonZeroIntegerTest4
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

parseTest :: Parser a -> Text -> Either String a
parseTest p = parseOnly (p <* endOfInput)

genComment :: Gen Text
genComment = do
  str <- (pack . filterSpaces) `liftM` arbitrary
  indent <- choose (0,100)
  return ((pack "c") `append` (pack (replicate indent ' ')) `append` str)

filterSpaces :: String -> String
filterSpaces [] = []
filterSpaces ('\n':xs) = filterSpaces xs
filterSpaces ('\r':xs) = filterSpaces xs
filterSpaces (x:xs) = x : filterSpaces xs

parseCommentGen :: String -> TestTree
parseCommentGen str =
  testCase ("parseComment \"" ++ str ++ "\"") $ assert (
    parseTest parseComment (pack str) == Right ()
    )

parseCommentTest1 :: TestTree
parseCommentTest1 =
  parseCommentGen "c hello world"

parseCommentTest2 :: TestTree
parseCommentTest2 =
  parseCommentGen "   c hello world"

parseCommentTest3 :: TestTree
parseCommentTest3 =
  testCase ("parseComment \"" ++ testStr ++ "\"") $ do
    let result = parseTest parseComment (pack testStr)
    assertBool ("Expected Left. Gotten: " ++ show result) (isLeft result)
  where
    testStr = "c hello world\nf"

parseCommentTest4 :: TestTree
parseCommentTest4 =
  testProperty "Parse random comments" $
  forAll
  genComment
  (\text ->
    parseTest parseComment text == Right ()
    )

parseCommentsTest1 :: TestTree
parseCommentsTest1 =
  testCase ("parseComments \"" ++ testStr ++ "\"") $ assert (
    parseTest parseComments (pack testStr) == Right ()
    )
  where
    testStr = "c hello world\nc goodbye world\n"

parseCommentsTest2 :: TestTree
parseCommentsTest2 =
  testProperty "parseComments randomly generated" $ forAll
  (do
      x <- choose (0,50)
      y <- replicateM x genComment
      return $ T.unlines y
      )
  (\text ->
    parseTest parseComments text === Right ()
    )

parseProblemLineTest1 :: TestTree
parseProblemLineTest1 =
  testCase ("parseProblemLine \"" ++ testStr ++ "\"") $ assert (
    parseTest parseProblemLine (pack testStr) == (Right . Right $ cnf')
    )
  where
    testStr = "p cnf 24 2424"
    cnf' = CNFBuilder 24 2424 0 emptyClauses emptyClause

parseProblemLineTest2 :: TestTree
parseProblemLineTest2 =
  testCase ("parseProblemLine \"" ++ testStr ++ "\"") $ assert (
    parseTest parseProblemLine (pack testStr) == ( Right . Right $ cnf')
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
    in parseTest parseProblemLine text === (Right . Right $ cnf')
       )
       
parseNonZeroIntegerTest1 :: TestTree
parseNonZeroIntegerTest1 =
  testCase ("parseNonZeroInteger \"" ++ testStr ++ "\"") $ assert (
    parseTest parseNonZeroInteger (pack testStr) ==
    (Right word)
    )
  where
    testStr = show word
    word = 121

parseNonZeroIntegerTest2 :: TestTree
parseNonZeroIntegerTest2 =
  testCase ("parseNonZeroInteger \"" ++ testStr ++ "\"") $ assert (
    parseTest parseNonZeroInteger (pack testStr) == Right word
    )
  where
    testStr = show word
    word = -121

parseNonZeroIntegerTest3 :: TestTree
parseNonZeroIntegerTest3 =
  testCase ("parseNonZeroInteger \"" ++ testStr ++ "\"") $ assert (
    isLeft $ parseTest parseNonZeroInteger (pack testStr)
    )
  where
    testStr = show word
    word = 0
    
parseNonZeroIntegerTest4 :: TestTree
parseNonZeroIntegerTest4 =
  testProperty "parseNonZeroInteger sucessful" $
  forAll
  mkIntegerNonZero
  (\word ->
    let exptd = return word
    in (parseTest parseNonZeroInteger (pack $ show word)) === exptd
       )

parseClauseTest1 :: TestTree
parseClauseTest1 =
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClause $ return cnf) (pack testStr) @=?
    (Right $ finishClause cnf')
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
    parseTest (parseClause $ return cnf) (pack testStr) ==
    (Right  $ finishClause $ cnf')
    )
    where
      cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
      cnf' = V.foldl (flip addLiteral') cnf . V.map literalToInteger $ (getVectLiteral cl)
      cl = mkClauseFromLits $ map mkLiteralFromInteger [
        1,2,3,-4,-5,6,-7,8,9
                           ]
      testStr = "1   2   3   -4 -5    6 -7   8            9 0"

parseClauseTest3 :: TestTree
parseClauseTest3 = 
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClause $ return cnf) (pack testStr) ==
    (Right $ finishClause cnf')
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
    parseTest (parseClause $ return cnf) (pack testStr) ==
    (Right $ finishClause cnf')
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
  genX
  (\(before,after,text,c) ->
    let gotten = parseTest (parseClause $ before) text
    in  gotten === (return  after)
        )


genX :: Gen (CNFBuildErr,CNFBuildErr,Text,Clause)
genX = do
  before <- return `liftM` genCNFBuilderEmptyClause 10 10 10 10
  clause <- arbitrary
  let lits = getVectLiteral clause
      after = (V.foldl (\b' l -> b' >>= addLiteral (literalToInteger l)) before lits) >>= finishClause
  text <- generateClause (V.map literalToInteger lits)
  return ( before,after,text,clause)

generateClause :: V.Vector Integer -> Gen Text
generateClause v = V.foldM generateClause' T.empty (V.snoc v 0)
  where
    generateClause' :: Text -> Integer -> Gen Text
    generateClause' t i = append t `liftM` oneof [
      empty'              >>=              showNumb i,
      empty' >>=  ret     >>=              showNumb i,
      empty' >>= addSpace >>=              showNumb i,
      empty' >>= ret      >>= addSpace >>= showNumb i,
      empty' >>= ret                   >>= showNumb i,
      empty' >>= addSpace >>= ret      >>= showNumb i,
      empty' >>= addSpace >>= ret      >>= addComs   >>= addSpace >>= showNumb i,
      empty' >>= addSpace >>= ret      >>= addComs   >>= showNumb i,
      empty' >>= ret      >>= addComs  >>= addSpace  >>= showNumb i,
      empty' >>= ret      >>= addComs  >>= showNumb i
      ]
    empty' :: Gen Text
    empty' = return T.empty
    ret,addSpace,addComs :: Text -> Gen Text
    ret t = return $ t `append` (pack "\n")
    addSpace t = do
      s <- genSpace
      return $ t `append` (pack s)
    addComs t = do
      x <- choose (0,50)
      y <- replicateM x genComment
      return $ t `append` (T.unlines y)
    showNumb :: Integer -> Text -> Gen Text
    showNumb i t = do
      s <- genSpace
      return $ t `append` (pack (show i ++ (if i==0 then
                                              "" else
                                              [(Prelude.head s)])))

parseClausesTest1 :: TestTree
parseClausesTest1 =
  testCase ("parseClauses \"" ++ testStr ++ "\"") $ assert (
    parseOnly (parseClauses $ return cnf) (pack testStr) @=?
    (Right cnf')
    )
  where
    testStr = "1 2 3 -4 -5 -6 0 " ++
              "-7 -8 9 10 0 " ++ "-1 -2 -3 -4 -7 0 " ++
              "1 1 1 1 1 1 0"
    cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = return $ CNFBuilder 10 10 4 madeClauses emptyClause
    madeClauses = mkClausesFromIntegers [
      [1,2,3,-4,-5,-6],
      [-7,-8,9,10],
      [-1,-2,-3,-4,-7],
      [1,1,1,1,1,1]
      ]

parseClausesTest2:: TestTree
parseClausesTest2=
  testCase ("parseClauses \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClauses $ return cnf) (pack testStr) @=?
    (Right cnf')
    )
  where
    testStr = "c the evil is hardest to find\n" ++
              "1 2 3 -4 -5 -6 0\n" ++
              "c when we can find the last one\n" ++
              "-7 -8 9 10 0\n" ++
              "c finding the last one is the easiest\n" ++
              "-1 -2 -3 -4 -7\nc intermitant\n " ++
              "0 1 1 1 1 1 1 0"
    cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = return $ CNFBuilder 10 10 4 madeClauses emptyClause
    madeClauses = mkClausesFromIntegers [
      [1,2,3,-4,-5,-6],
      [-7,-8,9,10],
      [-1,-2,-3,-4,-7],
      [1,1,1,1,1,1]
      ]

parseClausesTest3 :: TestTree
parseClausesTest3 =
  testProperty "parse randomly generated Clauses" $
  forAll
  madeClauses
  (\(before,after,text) ->
    let gotten = parseTest (parseClauses $ before) text
    in gotten === (return after)
       )

madeClauses :: Gen (CNFBuildErr,CNFBuildErr,Text)
madeClauses = do
  clauses <- Prelude.take 20 `liftM` listOf (
    listOf (do
    s <- arbitrary
    x <- choose (1,20)
    return $ if s then x else (negate x)
    )
    )
  let before = cnfBuilder 20 20
      after = return $ CNFBuilder 20 20 (toEnum $ Prelude.length clauses) (mkClausesFromIntegers clauses) emptyClause
  text <- genClausesText clauses
  return (before,after,text)

genClausesText :: [[Integer]] -> Gen Text
genClausesText xs = do
  header <- genComments
  middle <- genClausesText' xs
  footer <- genComments
  return $ header <> middle <> footer
  where
    genClausesText' :: [[Integer]] -> Gen Text
    genClausesText' [] = return mempty
    genClausesText' (x:xs) = do
      header <- genComments
      middle <- generateClause (V.fromList x)
      footer <- genComments
      rest <- genClausesText' xs
      return $ header <> middle <> footer <> rest

genComments :: Gen Text
genComments = do
  number <- choose (0,50)
  comments <- replicateM number genComment
  return $ T.unlines comments
