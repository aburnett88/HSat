module HSat.Parser (
  fromCNFFile
  ) where

import HSat.Problem

fromCNFFile :: FilePath -> Either ProblemParseError Problem
fromCNFFile fp = Left ProblemParseError



data ProblemParseError =
  ProblemParseError
  deriving (Eq,Show)
