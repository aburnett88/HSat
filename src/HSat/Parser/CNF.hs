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

import Data.Attoparsec.Text (Parser)
import HSat.Problem.BSP.CNF.Builder
import HSat.Problem.BSP.CNF
import HSat.Parser.CNF.Internal

cnfParser :: Parser (Either CNFBuilderError CNF)
cnfParser =
  parseProblemLine >>= parseClauses >>= (\builder -> return $ builder >>= finalise)
