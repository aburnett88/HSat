module HSat.Parser.CNF.Internal (
  parseComment,
  parseComments,
  parseNonZeroInteger,
  parseProblemLine,
  parseClause,
  parseClauses
  ) where

import Data.Attoparsec.Text as P
import HSat.Problem.BSP.CNF.Builder
import Control.Monad (void,liftM)
import Data.Text

parseComments :: Parser ()
parseComments =
  skipMany (parseComment >> endOfLine >> return ()) <?>
  "parseComments"

parseComment :: Parser ()
parseComment =
  many' space' >> P.char 'c' >> skipWhile (\a -> not $ isEndOfLine a ) <?>
  "parseComment"

parseProblemLine :: Parser CNFBuildErr
parseProblemLine = (do
  skipMany space'
  char 'p'
  skipMany1 space'
  string . pack $ "cnf"
  skipMany1 space'
  vars <- P.signed P.decimal
  skipMany1 space'
  clauses <- P.signed P.decimal
  skipMany space'
  return $ cnfBuilder vars clauses) <?> "parseProblemLine"

space' :: Parser ()
space' = void $ choice [char '\t', char ' ']

positive :: Parser Integer
positive = do
  x <- choices "123456789"
  xs <- many' $ choices "0123456789"
  return . read $ (x:xs)

choices :: String -> Parser Char
choices xs = choice $ fmap char xs

parseClause :: CNFBuildErr -> Parser CNFBuildErr
parseClause b = choice [
  do
    many' space'
    char '0'
    return $ b >>= finishClause,
  do
    many' space'
    b' <- (\i -> b >>= addLiteral i) `liftM` parseNonZeroInteger
    parseClause b',
  do
    many' space'
    endOfLine
    parseComments
    parseClause b
    ]
    
parseClauses :: CNFBuildErr -> Parser CNFBuildErr
parseClauses cnf = do
  parseComments >> choice [
    parseClause cnf >>= parseClauses,
    return cnf
    ]
                             

parseNonZeroInteger :: Parser Integer
parseNonZeroInteger = do
  f <- option id (char '-' >> return negate)
  x <- positive
  return $ f x
