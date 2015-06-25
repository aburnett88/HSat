{-# LANGUAGE
    OverloadedStrings
    #-}

{-|
Module      : HSat.Problem.Instances.CNF.Parser.Internal
Description : Helper functions for parsing CNF files
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports a set of internally used functions to parse parts of a 'CNF' file
-}

module HSat.Problem.Instances.CNF.Parser.Internal (
  -- * Functions
  parseComment,        -- :: Parser ()
  parseComments,       -- :: Parser ()
  parseNonZeroInteger, -- :: Parser Integer
  parseProblemLine,    -- :: Parser CNFBuildErr
  parseClause,         -- :: CNFBuildErr -> Parser CNFBuildErr
  parseClauses         -- :: CNFBuildErr -> Parser CNFBuildErr
  ) where

import           Control.Monad                      (void)
import           Control.Monad.Catch
import qualified Data.Attoparsec.Internal.Types     as T
import           Data.Attoparsec.Text               as P
import           HSat.Problem.Instances.CNF.Builder

instance MonadThrow (T.Parser i) where
  throwM e = fail (show e)

{-|
Parser to parse many comments. No information is retained about the comments
-}
parseComments :: Parser ()
parseComments = void (skipMany (parseComment >> endOfLine)) <?> "parseComments"

{-|
Parser to parse a comment. No information is retained about the comment.
-}
parseComment :: Parser ()
parseComment =
  many' space' >> option () (
    char 'c' >> skipWhile (not . isEndOfLine)) <?> "parseComment"

{-|
Parser to parse a problem line in a CNF file. Builds a 'CNFBuildErr' from it
-}
parseProblemLine :: (MonadThrow m) => Parser (m CNFBuilder) 
parseProblemLine = do
  vars <- 
    skipMany space' >> char 'p' >> skipMany1 space' >>
    string "cnf" >> skipMany1 space' >> P.signed P.decimal
  clauses <- skipMany1 space' >> P.signed P.decimal
  cnfBuilder vars clauses <$ skipMany space'
  <?> "parseProblemLine"

--Parsers spaces and tabs
space' :: Parser ()
space' = void (choice [char '\t', char ' ']) <?> "space'"

--Parses strictly positive numbers
positive :: Parser Integer
positive = (
  do
    x <- choices "123456789"
    xs <- many' $ choices "0123456789"
    return . read $ (x:xs)
  ) <?> "positive"

--A helper function. When given a string, will parse any of the characters
choices    :: String -> Parser Char
choices xs = choice (fmap char xs) <?> "choices"

{-|
Parser to parse a 'Clause', adding the 'HSat.Problem.BSP.Common.Clause' to the 'CNFBuildErr' argument
-}
parseClause   :: (MonadThrow m) => m CNFBuilder -> Parser (m CNFBuilder)
parseClause b = choice [
  --Parse the '0' at the end of the clause
  (b >>= finishClause) <$ (many' space' >> char '0'),
  --Parse a non zero integer, then continue
  (many' space' >> (\i -> b >>= addLiteral i) <$> parseNonZeroInteger) >>= parseClause,
  --Parse the end of the line, and any comments, then continue parsing the clause
  many' space' >> endOfLine >> parseComments >> parseClause b
  ] <?> "parseClause"

{-|
Parser to Parse a set of 'HSat.Problem.BSP.Common.Clauses', adding them to the 'CNFBuildErr' argument
-}
parseClauses     :: (MonadThrow m) => m CNFBuilder -> Parser (m CNFBuilder)
parseClauses cnf =
  parseComments >> choice [
    parseClause cnf >>= parseClauses,
    cnf <$ parseComments
    ] <?> "parseClauses"
                             
{-|
Parser to parse an 'Integer' that is non-zero
-}
parseNonZeroInteger :: Parser Integer
parseNonZeroInteger = option id (negate <$ char '-') <*> positive <?> "parseNonZeroInteger"
