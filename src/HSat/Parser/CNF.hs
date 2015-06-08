{-|
Module      : HSat.Parser.CNF
Description : The Parser for the CNF file format
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Module containing the 'Parser' for the CNF file format
-}

module HSat.Parser.CNF (
  cnfParser -- :: Parser (Either CNFBuilderError CNF)
  ) where

import Data.Attoparsec.Text         (Parser,endOfInput)
import HSat.Parser.CNF.Internal
import HSat.Problem.BSP.CNF
import HSat.Problem.BSP.CNF.Builder

{-|
Parser that parses a CNF file in 'Data.Text' form, and produces either a 'CNFBuilderError' or a 'CNF'
-}
cnfParser :: Parser (Either CNFBuilderError CNF)
cnfParser =
  (finalise =<<) <$> (
    parseComment >> parseComments >> parseProblemLine >>= parseClauses) <* endOfInput
