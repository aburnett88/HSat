{-|
Module      : HSat.Parser.CNF
Description : The Parser for the CNF file format
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Module containing the 'Parser' for the CNF file format
-}

module HSat.Problem.Instances.CNF.Parser (
  cnfParser -- :: Parser (Either CNFBuilderError CNF)
  ) where

import Data.Attoparsec.Text         (Parser,endOfInput)
import HSat.Problem.Instances.CNF.Parser.Internal
import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.CNF.Builder
import Control.Monad.Catch

{-|
Parser that parses a CNF file in 'Data.Text' form, and produces either a 'CNFBuilderError' or a 'CNF'
-}
cnfParser :: (MonadThrow m) => Parser (m CNF)
cnfParser =
  (finalise =<<) <$> (
    parseComment >> parseComments >> parseProblemLine >>= parseClauses) <* endOfInput
