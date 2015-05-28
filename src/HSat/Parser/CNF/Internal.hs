{-|
Module      : HSat.Parser.CNF.Internal
Description : Interanl functions to parse 'CNF' files
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports a set of internally used functions to parse parts of a 'CNF' file
-}

module HSat.Parser.CNF.Internal (
  -- * Functions
  parseComment,
  parseComments,
  parseNonZeroInteger,
  parseProblemLine,
  parseClause,
  parseClauses
  ) where

import Data.Attoparsec.Text as P
import HSat.Problem.BSP.CNF.Builder
import Control.Monad (void)
import Data.Text

{-|
Parse a list of comments. No information is retained
-}
parseComments :: Parser ()
parseComments =
  void (skipMany (parseComment >> endOfLine))

{-|
Skips comment lines, or lines with no text on them (only tabs and spaces)
-}
parseComment :: Parser ()
parseComment =
  many' space' >> option () (
    char 'c' >> skipWhile (not . isEndOfLine))

{-|
Parses the problem line, and returns an initialised 'CNFBuilder' or an error
-}
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

{-|
Parses a 'Clause' into a 'CNFBuilder', or throws the error
-}
parseClause :: CNFBuildErr -> Parser CNFBuildErr
parseClause b = choice [
  do
    _ <- many' space'
    _ <- char '0'
    return $ b >>= finishClause,
  do
    _ <- many' space'
    b' <- (\i -> b >>= addLiteral i) <$> parseNonZeroInteger
    parseClause b',
  do
    _ <- many' space'
    endOfLine
    parseComments
    parseClause b
    ]

{-|
Parses a set of 'Clauses' into a 'CNFBuilder' or throws the error
-}
parseClauses :: CNFBuildErr -> Parser CNFBuildErr
parseClauses cnf =
  parseComments >> choice [
    parseClause cnf >>= parseClauses,
    parseComments >> return cnf
    ]
                             
{-|
Parses a non-negative 'Integer'
-}
parseNonZeroInteger :: Parser Integer
parseNonZeroInteger = do
  f <- option id (char '-' >> return negate)
  x <- positive
  return $ f x
