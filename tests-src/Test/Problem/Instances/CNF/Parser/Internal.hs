{-# LANGUAGE
    OverloadedStrings
    #-}

{-|
Module      : HSat.Problem.Instances.CNF.Parser.Internal
Description : Exports the Internal tests for the CNF Parser
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Module containing the Internal 'Parser' tests for the 'CNF' data type
-}

module Test.Problem.Instances.CNF.Parser.Internal (
  tests, -- TestTree
  ) where

import           Control.Applicative
import           Control.Monad                               (replicateM,foldM)
import           Control.Monad.Catch
import           Data.Attoparsec.Text                        as P hiding (parseTest,take)
import           Data.Either
import           Data.Monoid
import           Data.Text                                   (Text,pack,take)
import qualified Data.Text                                   as T hiding (map,replicate,take,foldl)
import qualified Data.Vector                                 as V
import           HSat.Parser                                 hiding (Parser)
import           HSat.Problem.Instances.CNF.Builder
import           HSat.Problem.Instances.CNF.Builder.Internal
import           HSat.Problem.Instances.CNF.Parser.Internal
import           HSat.Problem.Instances.Common
import           Prelude                                     hiding (take)
import           Test.Problem.Instances.CNF.Builder.Internal hiding (tests)
import           Test.Problem.Instances.CNF.Internal ()
import           Test.Problem.Instances.Common.Clauses       hiding (tests)
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

--General function used to parse all the input given
parseTest   :: Parser a -> Text -> Either String a
parseTest p = parseOnly (p <* endOfInput)

parseTest'     :: (MonadThrow m) => Parser (m a) -> Text -> m a
parseTest' p t = case parseTest p t of
  Left str -> throwM $ ParseException str
  Right r -> r
{-
===================
 parseComment Tests
===================
-}
--General framework for correct Test Cases
parseCommentGen                 :: String -> Text -> Bool -> TestTree
parseCommentGen title str True  =
  testCase title $ parseTest parseComment str @=? Right ()
parseCommentGen title str False =
  testCase title $ case parseTest parseComment str of
   Left err -> assertBool ("Expected success. Gotten error: " ++ show err) False
   Right _ -> assertBool "" True

parseCommentTest1 :: TestTree
parseCommentTest1 =
  parseCommentGen
    "parse simple comment"
    "c hello world"
    True

parseCommentTest2 :: TestTree
parseCommentTest2 =
  parseCommentGen
    "Parse comment with additional spaces"
    "   c hello world"
    True

parseCommentTest3 :: TestTree
parseCommentTest3 =
  parseCommentGen
    "Parse Comment that fails"
    "c hello world\nf"
    False

-- Generate a random comment
genComment   :: Int -> Gen Text
genComment _ = ("c " <>) . T.filter f <$> arbitrary
  where
    f   :: Char -> Bool
    f t = t /= '\n' && t/='\r'

parseCommentTest4 :: TestTree
parseCommentTest4 =
  testProperty "Parse random comments" $
  forAll
  (sized genComment)
  (\text ->
    parseTest parseComment text === Right ()
  )

{-
===================
parseComments tests
===================
-}
parseCommentsTest1 :: TestTree
parseCommentsTest1 =
  testCase "parse valid comments" $
    parseTest parseComments testStr @=? Right ()
  where
    testStr = "c hello world\nc goodbye world\n"

parseCommentsTest2 :: TestTree
parseCommentsTest2 =
  testProperty "parseComments randomly generated" $ forAll
  (sized $ \size ->
    T.unlines <$> (flip replicateM (genComment size) =<< choose (0,size))
  )
  (\text ->
    parseTest parseComments text === Right ()
  )

{-
======================
parseProblemLine tests
======================
-}

genProblemLineTest               :: String -> Text -> TestTree
genProblemLineTest title testStr =
  testCase title $
  case parseTest' parseProblemLine testStr of
    Right cnf -> cnf @=? cnf'
    _         -> assertBool "Failure; exception thrown" False
  where
    cnf' = CNFBuilder 24 2424 0 emptyClauses emptyClause
  
parseProblemLineTest1 :: TestTree
parseProblemLineTest1 =
  genProblemLineTest "Parse normal problem line" "p cnf 24 2424"

parseProblemLineTest2 :: TestTree
parseProblemLineTest2 =
  genProblemLineTest "Parse problem line with spaces"
   "   p    cnf    24 \t2424"

genSpace'             :: Int -> Int -> Gen Text
genSpace' offSet size =
  pack <$> (flip replicateM (oneof . map return $ " \t") =<<
  (offSet +) <$> choose (0,size))

genSpace :: Int -> Gen Text
genSpace = genSpace' 0

parseProblemLineTest3 :: TestTree
parseProblemLineTest3 =
  testProperty "parseProblemLine parses correctly" $ forAll
  (sized $ \size -> do
      vars    <- arbitrary
      clauses <- arbitrary
      let genSpace0 = genSpace' 1 size
          vars'     = return $ pack . show $ vars
          clauses'  = return $ pack . show $ clauses
          p         = return "p"
          cnf       = return "cnf"
      text <- mconcat <$> sequence [
        p,genSpace0,cnf,genSpace0,vars',genSpace0,clauses',genSpace size]
      return (text,vars,clauses)
  )
  (\(text,v,c) ->
    let cnf' = CNFBuilder v c 0 emptyClauses emptyClause
    in case parseTest' parseProblemLine text of
        Right expected -> counterexample "Not same" $ expected === cnf'
        _ -> counterexample "Unexpected exception" False
  )

{-
=========================
parseNonZeroInteger tests
=========================
-}

genParseNonZeroInteger                 :: String ->  Integer -> Bool -> TestTree
genParseNonZeroInteger title word True =
  testCase title $
  case parseTest parseNonZeroInteger (pack $ show word) of
   Right word' -> word' @=? word
   _           -> assertBool "Unexpected exception" False
genParseNonZeroInteger title word False =
  testCase title $
  assertBool "Expected exception" (isLeft $ parseTest parseNonZeroInteger (pack $ show word))
  
parseNonZeroIntegerTest1 :: TestTree
parseNonZeroIntegerTest1 =
  genParseNonZeroInteger "Parses correct non zero integer" 121 True

parseNonZeroIntegerTest2 :: TestTree
parseNonZeroIntegerTest2 =
  genParseNonZeroInteger "Parses correct non zero integer" 2453565 True

parseNonZeroIntegerTest3 :: TestTree
parseNonZeroIntegerTest3 =
  genParseNonZeroInteger "Doesn't parse zero" 0 False
    
parseNonZeroIntegerTest4 :: TestTree
parseNonZeroIntegerTest4 =
  testProperty "parseNonZeroInteger successful" $
  forAll
  mkIntegerNonZero
  (\word ->
    parseTest parseNonZeroInteger (pack $ show word) === return word
  )

{-
=================
parseClause tests
=================
-}

genParseClauseTest               :: String -> Text -> TestTree
genParseClauseTest title testStr =
  testCase title $
  case (parseTest' (parseClause $ return cnf) testStr, finishClause cnf') of
   (Just gotten, Just exptd) -> gotten @=? exptd
   _                         -> assertBool "Unexpected failure" False
  where
    cnf  = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = V.foldl (flip addLiteral') cnf . V.map literalToInteger $
           getVectLiteral cl
    cl   = mkClauseFromLits $ map mkLiteralFromInteger [
      1,2,3,-4,-5,6,-7,8,9]

parseClauseTest1 :: TestTree
parseClauseTest1 =
  genParseClauseTest "parse good clause" "1 2 3 -4 -5 6 -7 8 9 0"

parseClauseTest2 :: TestTree
parseClauseTest2 =
  genParseClauseTest "Parse clause with spaces" "1   2   3   -4 -5    6 -7   8            9 0"

parseClauseTest3 :: TestTree
parseClauseTest3 =
  genParseClauseTest "Parse Clause with new lines"  "1   2   3   -4 -5 \n   6 -7   8    \n        9 0"

parseClauseTest4 :: TestTree
parseClauseTest4 =
  genParseClauseTest "Parse clause with comments in between lines" $
  "1   2 \n"        <> "c initial\n"     <> "3   -4 -5 \n"    <>
  "c hello world\n" <> "6 -7   8    \n"  <> "9 0"

parseClauseTest5 :: TestTree
parseClauseTest5 =
  testProperty "parse randomly generated clauses" $
  forAll
  (sized $ \size -> do
      beforeBuilder <- return <$> genCNFBuilderEmptyClause size :: Gen (Maybe CNFBuilder)
      clause        <- arbitrary
      let lits         = clauseToIntegers clause
          afterBuilder = foldl (\b l -> b >>= addLiteral l) beforeBuilder lits >>= finishClause
      text          <- generateClause size lits
      return (beforeBuilder,afterBuilder,text)
  )
  (\(before,after,text) ->
    let gotten = parseTest (parseClause before) text
    in  gotten === return after
  )

generateClause           :: Int -> [Integer] -> Gen Text
generateClause size ints = foldM generateClause' T.empty $ ints ++ [0]
  where
    generateClause'          :: Text -> Integer -> Gen Text
    generateClause' text int = (text <>) <$> (
      oneof . map (\f -> f >>= showNumb int) $ [
         empty',
         empty' >>= ret,
         empty' >>= addSpace,
         empty' >>= ret      >>= addSpace,
         empty' >>= addSpace >>= ret,
         empty' >>= addSpace >>= ret     >>= addComs >>= addSpace,
         empty' >>= addSpace >>= ret     >>= addComs,
         empty' >>= ret      >>= addComs >>= addSpace,
         empty' >>= ret      >>= addComs
         ]
      )
    empty'               :: Gen Text
    empty' = return mempty
    ret,addSpace,addComs :: Text -> Gen Text
    ret t = return $ t <> pack "\n"
    addSpace t = (t <>) <$> genSpace' 1 size
    addComs t = (t <>) . T.unlines <$>
                (choose (0,size) >>= flip replicateM (genComment size))
    showNumb             :: Integer -> Text -> Gen Text
    showNumb i t = do
      s <- genSpace' 1 size
      return $ s <> t <> (pack . show $ i) <> (if i==0 then const "" else take 1) s

{-
==================
parseClauses tests
==================
-}

genParseClauses               :: String -> Text -> TestTree
genParseClauses title testStr =
  testCase title $
  case (parseTest' (parseClauses $ return cnf) testStr,cnf') of
    (Just gotten, Just expected) -> expected @=? gotten
    _                            -> assertBool "Unexpected error" False
  where
    cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = return $ CNFBuilder 10 10 4 clauses emptyClause
    clauses = mkClausesFromIntegers [
      [1,2,3,-4,-5,-6],
      [-7,-8,9,10],
      [-1,-2,-3,-4,-7],
      [1,1,1,1,1,1]
      ]

parseClausesTest1 :: TestTree
parseClausesTest1 =
  genParseClauses "parse generic clauses" $
  "1 2 3 -4 -5 -6 0 " <> "-7 -8 9 10 0 " <>
  "-1 -2 -3 -4 -7 0 " <> "1 1 1 1 1 1 0"

parseClausesTest2 :: TestTree
parseClausesTest2 =
  genParseClauses "parse clauses with comments" $
  "c the evil is hardest to find\n"         <> "1 2 3 -4 -5 -6 0\n" <>
  "c when we can find the last one\n"       <> "-7 -8 9 10 0\n"     <>
  "c finding the last one is the easiest\n" <>
  "-1 -2 -3 -4 -7\nc intermitant\n "        <> "0 1 1 1 1 1 1 0"

parseClausesTest3 :: TestTree
parseClausesTest3 =
  testProperty "parse randomly generated Clauses" $
  forAll
  (sized $ \size -> do
      clauses <- genClauses maxBound size
      let setSize = getSizeClauses clauses
          maxVar  = findMaxVar clauses
          before  = return $ CNFBuilder maxVar setSize       0 emptyClauses emptyClause
          after   = return $ CNFBuilder maxVar setSize setSize      clauses emptyClause
          f       = round . log :: Double -> Int
          size'   = f $ fromIntegral size
      text <- genClausesText size' clauses
      return (before, after, text)
  )
  (\(before,after,text) ->
    let gotten = parseTest' (parseClauses before) text
    in case (gotten,after) of
        (Just gotten',Just after') -> counterexample "Clauses not same: " $ gotten' === after'
        _ -> counterexample "Unexpected exception" False
  )
  where
    genClausesText     :: Int -> Clauses -> Gen Text
    genClausesText size clauses = do
      header <- genComments size
      middle <- genClausesText' (clausesToIntegers clauses)
      footer <- genComments size
      return $ header <> middle <> footer
      where
        genClausesText' :: [[Integer]] -> Gen Text
        genClausesText' [] = return mempty
        genClausesText' (x:xs) = do
          header <- genComments size
          middle <- generateClause size x
          footer <- genComments size
          rest <- genClausesText' xs
          return $ header <> middle <> footer <> rest
    genComments      :: Int -> Gen Text
    genComments size = do
      m <- choose (0,size)
      T.unlines <$> replicateM m (genComment size)
