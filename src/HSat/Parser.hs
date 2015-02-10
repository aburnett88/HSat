module HSat.Parser (
  fromCNFFile
  ) where

import HSat.Problem

fromCNFFile :: FilePath -> Either ProblemParseError Problem
fromCNFFile fp = undefined

data ProblemParseError =
  ProblemParseError
  deriving (Eq,Show)
