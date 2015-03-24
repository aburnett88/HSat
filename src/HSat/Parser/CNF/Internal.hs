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
  skipMany (parseComment >> endOfLine) >> return ()

{-|
Skips comment lines, or lines with no text on them (only tabs and spaces)
-}
parseComment :: Parser ()
parseComment = do
  many' space' >> option () (
    char 'c' >> skipWhile (not . isEndOfLine))

parseProblemLine :: Parser CNFBuildErr
parseProblemLine = (do
  skipMany space'
  _ <- char 'p'
  skipMany1 space'
  _ <- string . pack $ "cnf"
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
    _ <- many' space'
    _ <- char '0'
    return $ b >>= finishClause,
  do
    _ <- many' space'
    b' <- (\i -> b >>= addLiteral i) `liftM` parseNonZeroInteger
    parseClause b',
  do
    _ <- many' space'
    endOfLine
    parseComments
    parseClause b
    ]
    
parseClauses :: CNFBuildErr -> Parser CNFBuildErr
parseClauses cnf = do
  parseComments >> choice [
    parseClause cnf >>= parseClauses,
    parseComments >> return cnf
    ]
                             

parseNonZeroInteger :: Parser Integer
parseNonZeroInteger = do
  f <- option id (char '-' >> return negate)
  x <- positive
  return $ f x
