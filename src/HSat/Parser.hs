module HSat.Parser (
  fromCNFFile,
  fromFile,
  fromFolder
  ) where

import HSat.Problem

type ReadFile = Either ProblemParseError Problem

fromCNFFile :: FilePath -> IO ReadFile
fromCNFFile fp = return $ Left ProblemParseError

fromFile :: FilePath -> IO ReadFile
fromFile fp = return $ Left ProblemParseError

fromFolder :: (FilePath -> IO ReadFile) -> FilePath -> IO [ReadFile]
fromFolder _ _ = return []

data ProblemParseError =
  ProblemParseError
  deriving (Eq,Show)
