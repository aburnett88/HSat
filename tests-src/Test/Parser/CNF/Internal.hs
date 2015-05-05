module Test.Parser.CNF.Internal (
  tests
  ) where

import           Control.Monad (replicateM,foldM)
import           Data.Attoparsec.Text as P hiding (parseTest,take)
import           Data.Either
import qualified Data.Text as T hiding (map,replicate,take,foldl)
import Data.Text (Text,pack)
import qualified Data.Vector as V
import           HSat.Parser.CNF.Internal
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.CNF.Builder.Internal
import           TestUtils
import Control.Applicative
import HSat.Problem.BSP.Common
import Test.Problem.BSP.CNF.Builder.Internal hiding (tests)
import Data.Monoid
import Test.Problem.BSP.Common.Clauses hiding (tests)

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

parseCommentTest1 :: TestTree
parseCommentTest1 =
  parseCommentGen
    "parse simple comment"
    "c hello world"

parseCommentTest2 :: TestTree
parseCommentTest2 =
  parseCommentGen
    "Parse comment with additional spaces"
    "   c hello world"

--General framework for above two tests
parseCommentGen           :: String -> String -> TestTree
parseCommentGen title str =
  testCase title $ parseTest parseComment (pack str) @=? Right ()

parseCommentTest3 :: TestTree
parseCommentTest3 =
  testCase ("parseComment \"" ++ testStr ++ "\"") $ do
    let result = parseTest parseComment (pack testStr)
    assertBool ("Expected Left. Gotten: " ++ show result) (isLeft result)
  where
    testStr = "c hello world\nf"


genComment :: Int -> Gen Text
genComment _ = do
  str <- genStr
  let start = pack "c "
  return $ start <> str
  where
    genStr :: Gen Text
    genStr = T.filter f `liftA` arbitrary
    f :: Char -> Bool
    f t = t /= '\n' && t/='\r'

parseCommentTest4 :: TestTree
parseCommentTest4 =
  testProperty "Parse random comments" $
  forAll
  (sized genComment)
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
  (sized $ \size -> do
      x <- choose (0,size)
      y <- replicateM x (genComment size)
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

genSpace' :: Int -> Int -> Gen String
genSpace' offSet size = do
  n <- (offSet +) `liftA` choose (0,size)
  replicateM n $
    oneof [
       return ' ',
       return '\t'
       ]

genSpace :: Int -> Gen String
genSpace = genSpace' 0

parseProblemLineTest3 :: TestTree
parseProblemLineTest3 =
  testProperty "parseProblemLine parses correctly" $ forAll
  (sized $ \size -> do
      vars <- arbitrary
      clauses <- arbitrary
      let genSpace0 = genSpace' 1 size
      text <- do
        let p = "p"
            cnf = "cnf"
            v' = show vars
            c' = show clauses
        spc1 <- genSpace0
        spc2 <- genSpace0
        spc3 <- genSpace0
        spc4 <- genSpace size
        return $
          p ++ spc1 ++
          cnf ++ spc2 ++
          v' ++ spc3 ++
          c' ++ spc4
      return (pack text, vars, clauses)
      )
  (\(text,v,c) ->
    let cnf' = CNFBuilder v c 0 emptyClauses emptyClause
    in parseTest parseProblemLine text === (Right . Right $ cnf')
       )
       
parseNonZeroIntegerTest1 :: TestTree
parseNonZeroIntegerTest1 =
  testCase ("parseNonZeroInteger \"" ++ testStr ++ "\"") $ assert (
    parseTest parseNonZeroInteger (pack testStr) ==
    Right word
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
    word :: Integer
    word = 0
    
parseNonZeroIntegerTest4 :: TestTree
parseNonZeroIntegerTest4 =
  testProperty "parseNonZeroInteger sucessful" $
  forAll
  mkIntegerNonZero
  (\word ->
    let exptd = return word
    in parseTest parseNonZeroInteger (pack $ show word) === exptd
       )

parseClauseTest1 :: TestTree
parseClauseTest1 =
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClause $ return cnf) (pack testStr) @=?
    (Right $ finishClause cnf')
    )
  where
    (cnf,cnf') = getTuple
    testStr = "1 2 3 -4 -5 6 -7 8 9 0"
    

parseClauseTest2 :: TestTree
parseClauseTest2 =
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClause $ return cnf) (pack testStr) ==
    (Right $ finishClause cnf')
    )
    where
      (cnf,cnf') = getTuple
      testStr = "1   2   3   -4 -5    6 -7   8            9 0"

parseClauseTest3 :: TestTree
parseClauseTest3 = 
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClause $ return cnf) (pack testStr) ==
    (Right $ finishClause cnf')
    )
    where
      (cnf,cnf') = getTuple
      testStr = "1   2   3   -4 -5 \n   6 -7   8    \n        9 0"

parseClauseTest4 :: TestTree
parseClauseTest4 = 
  testCase ("parseClause \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClause $ return cnf) (pack testStr) ==
    (Right $ finishClause cnf')
    )
    where
      (cnf,cnf') = getTuple
      testStr = unlines [
        "1   2 ",
        "c initial",
        "3   -4 -5 ",
        "c hello world",
        "6 -7   8    ",
        "9 0"
        ]

getTuple :: (CNFBuilder,CNFBuilder)
getTuple = (cnf,cnf')
  where
    cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = V.foldl (flip addLiteral') cnf . V.map literalToInteger $
           getVectLiteral cl
    cl = mkClauseFromLits $ map mkLiteralFromInteger [
      1,2,3,-4,-5,6,-7,8,9]
    

--282

parseClauseTest5 :: TestTree
parseClauseTest5 =
  testProperty "parse randomly generated clauses" $
  forAll
  (sized genX)
  (\(before,after,text) ->
    let gotten = parseTest (parseClause before) text
    in  gotten === return after
        )

genX :: Int -> Gen (CNFBuildErr,CNFBuildErr,Text)
genX size = do
  before <- return `liftA` genCNFBuilderEmptyClause 10
  clause <- arbitrary
  let lits = clauseToIntegers clause
      after = foldl (\b' l -> b' >>= addLiteral l) before lits >>= finishClause
  text <- generateClause size lits
  return (before,after,text)

generateClause :: Int -> [Integer] -> Gen Text
generateClause size ints = foldM generateClause' T.empty ((++) ints [0])
  where
    generateClause' :: Text -> Integer -> Gen Text
    generateClause' t int = (t <>) `liftA` oneof [
      empty' >>= showNumb int,
      empty' >>= ret >>= showNumb int,
      empty' >>= addSpace >>= showNumb int,
      empty' >>= ret >>= addSpace >>= showNumb int,
      empty' >>= ret >>= showNumb int,
      empty' >>= addSpace >>= ret >>= showNumb int,
      empty' >>= addSpace >>= ret >>= addComs >>= addSpace >>= showNumb int,
      empty' >>= addSpace >>= ret >>= addComs >>= showNumb int,
      empty' >>= ret >>= addComs >>= addSpace >>= showNumb int,
      empty' >>= ret >>= addComs >>= showNumb int
      ]
    empty' :: Gen Text
    empty' = return mempty
    ret,addSpace,addComs :: Text -> Gen Text
    ret t = return $ t <> pack "\n"
    addSpace t = (\a -> t <> pack a) `liftA` genSpace' 1 size
    addComs t = (\a -> t <> T.unlines a) `liftA`
                (choose (0,size) >>= flip replicateM (genComment size))
    showNumb :: Integer -> Text -> Gen Text
    showNumb i t = do
      s <- genSpace' 1 size
      return $ t <> pack ((++) (show i)
                          ((if i==0 then const "" else take 1) s))

parseClausesTest1 :: TestTree
parseClausesTest1 =
  testCase ("parseClauses \"" ++ testStr ++ "\"") $ assert (
    parseOnly (parseClauses $ return cnf) (pack testStr) @=?
    Right cnf'
    )
  where
    testStr = "1 2 3 -4 -5 -6 0 " ++
              "-7 -8 9 10 0 " ++ "-1 -2 -3 -4 -7 0 " ++
              "1 1 1 1 1 1 0"
    (cnf,cnf') = getTripple

getTripple :: (CNFBuilder,Either l CNFBuilder)
getTripple = (cnf,cnf')
  where
    cnf = CNFBuilder 10 10 0 emptyClauses emptyClause
    cnf' = return $ CNFBuilder 10 10 4 clauses emptyClause
    clauses = mkClausesFromIntegers [
      [1,2,3,-4,-5,-6],
      [-7,-8,9,10],
      [-1,-2,-3,-4,-7],
      [1,1,1,1,1,1]
      ]

parseClausesTest2:: TestTree
parseClausesTest2=
  testCase ("parseClauses \"" ++ testStr ++ "\"") $ assert (
    parseTest (parseClauses $ return cnf) (pack testStr) @=?
    Right cnf'
    )
  where
    testStr = "c the evil is hardest to find\n" ++
              "1 2 3 -4 -5 -6 0\n" ++
              "c when we can find the last one\n" ++
              "-7 -8 9 10 0\n" ++
              "c finding the last one is the easiest\n" ++
              "-1 -2 -3 -4 -7\nc intermitant\n " ++
              "0 1 1 1 1 1 1 0"
    (cnf,cnf') = getTripple

parseClausesTest3 :: TestTree
parseClausesTest3 =
  testProperty "parse randomly generated Clauses" $
  forAll
  (sized madeClauses)
  (\(before,after,text) ->
    let gotten = parseTest (parseClauses before) text
    in gotten === return after
       )

madeClauses :: Int -> Gen (CNFBuildErr,CNFBuildErr,Text)
madeClauses size = do
  clauses <- genClauses maxBound size
  let setSize = getSizeClauses clauses
      setSize' = toInteger setSize
      maxVar = findMaxVar clauses
      maxVar' = toInteger maxVar
      before = cnfBuilder maxVar' setSize'
      after = return $ CNFBuilder maxVar setSize setSize clauses emptyClause
      f = round . log :: Double -> Int
      size' = f $ fromIntegral size
  text <- genClausesText size' clauses
  return (before,after,text)

genClausesText :: Int -> Clauses -> Gen Text
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

genComments :: Int -> Gen Text
genComments size = do
  m <- choose (0,size)
  T.unlines `liftA` replicateM m (genComment size)
