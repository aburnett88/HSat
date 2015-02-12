module HSat.Parser.CNF.Internal (
  parseComment,
  parseComments,
  parseLiteral,
  parseProblemLine,
  parseClause,
  parseClauses
  ) where

import Data.Attoparsec.Text as P
import HSat.Problem.BSP.CNF.Builder
import Data.Word
import Control.Monad (void)
import Data.Text
import HSat.Problem.BSP.Common

parseComments :: Parser ()
parseComments =
  skipMany (parseComment >> endOfLine >> return ())

parseComment :: Parser ()
parseComment =
  many' space >> P.char 'c' >> skipWhile (/='\n')

parseProblemLine :: Parser CNFBuildErr
parseProblemLine = do
  skipMany space'
  char 'p'
  skipMany1 space'
  string . pack $ "cnf"
  skipMany1 space'
  vars <- P.decimal
  skipMany1 space'
  clauses <- P.decimal
  return $ cnfBuilder vars clauses

space' :: Parser ()
space' = void $ choice [char '\t', char ' ']

positive :: Parser Word
positive = do
  x <- choices "123456789"
  xs <- many' . choices $ "0123456789"
  return . read $ (x:xs)

choices :: String -> Parser Char
choices xs = choice $ fmap char xs

parseClause :: CNFBuildErr -> Parser CNFBuildErr
parseClause build = return $ build

parseClauses :: CNFBuilder -> Parser CNFBuilder
parseClauses cnf = return $ cnf

parseLiteral :: Parser Literal
parseLiteral = do
  x <- positive
  s <- return $ True
  return $ mkLiteral (mkSign s) (mkVariable x)
