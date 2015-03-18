{-|
Module      : HSat.Parser.CNF
Description : The Parser for the CNF file format
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Parses CNF files
-}

module HSat.Parser.CNF (
  cnfParser
  ) where

import Data.Attoparsec.Text (Parser,endOfInput,skipMany,char)
import HSat.Problem.BSP.CNF.Builder
import HSat.Problem.BSP.CNF
import HSat.Parser.CNF.Internal

cnfParser :: Parser (Either CNFBuilderError CNF)
cnfParser = do
  parseComments
  b2 <-  parseProblemLine
  b3 <- parseClauses b2
  let b4 = b3 >>= finalise
  parseComments
  skipMany (char '\n')
  endOfInput
  return $ b4
