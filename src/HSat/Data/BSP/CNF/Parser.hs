{-|
Module      : HSat.Data.BSP.CNF.Parser
Description : The Parser for the CNF file format
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Parses CNF files
-}

module HSat.Data.BSP.CNF.Parser (
  parseCNF,
  parseMaybe,
  Resultant
  ) where

import HSat.Data.BSP.CNF (CNF)
import qualified HSat.Data.BSP.CNF as C
import qualified Data.Attoparsec.Text as P
import HSat.Data.BSP.CNF.Builder
import Data.Text
import qualified Data.Text.IO as T
import Control.Monad (void)

{-|
The resultant type from the parser. 
-}
type Resultant = Either Error CNF

{-|
Takes a filepath and constructs a CNF expresison from that file, if it is
within the correct data type. 
-}
parseCNF :: FilePath -> IO Resultant
parseCNF s = do
  contents <- T.readFile s
  return $ case P.parseOnly parser contents of
    Left str -> Left . OtherError $ str
    Right res -> res >>= finalise

{-|
Returns a Nothing if the parsing is unsucessful. 
-}
parseMaybe :: FilePath -> IO (Maybe CNF)
parseMaybe s = do
  cnf <- parseCNF s
  return $ case cnf of
   Left _ -> Nothing
   Right just -> Just just

parser :: P.Parser BuildErr
parser = parseComments >> parseProblemLine

parseComments :: P.Parser ()
parseComments =
  P.skipMany (parseComment >> P.endOfLine >> return ())

parseComment :: P.Parser ()
parseComment =
  P.many' space >> P.char 'c' >> P.skipWhile (not . P.isEndOfLine)

parseProblemLine :: P.Parser BuildErr
parseProblemLine = do
  P.skipMany space
  P.char 'p'
  P.skipMany1 space
  P.string . pack $ "cnf"
  P.skipMany1 space
  vars <- P.decimal
  P.skipMany1 space
  clauses <- P.decimal
  let cnf = cnfBuilder vars clauses
  parseClauses cnf

parseClauses :: BuildErr -> P.Parser BuildErr
parseClauses cnf = P.choice [
  do
    P.char '\n'
    cnf' <- parseLits cnf
    P.char '0'
    parseClauses (cnf' >>= finishClause)
  ,
  do
    P.char '\n'
    cnf' <- parseLits cnf
    parseClauses cnf'
  ,
  do
    P.endOfLine
    parseComment
    parseClauses cnf
  ,
  P.endOfInput >> return cnf
  ,
  do
    P.endOfLine
    P.char '%'
    P.endOfLine
    P.char '0'
    P.endOfInput >> return cnf
  ]

parseLits :: BuildErr -> P.Parser BuildErr
parseLits cnf = P.choice [
  do
    P.many' space
    func <- P.option id (
      do
        P.char '-'
        return negate
        )
    literal <- positive
    P.many' space
    parseLits (cnf >>= flip addLiteral (func literal))
  ,
  return cnf
  ]

space :: P.Parser ()
space =
  void $ P.choice [P.char '\t',P.char ' ']
  
positive :: (Integral a, Read a) => P.Parser a
positive = do
  x <- choices "123456789"
  xs <- P.many' . choices $ "0123456789"
  return . read $ (x:xs)


choices :: String -> P.Parser Char
choices xs = P.choice $ fmap P.char xs
            
