{-|
Module      : Parsing.Main
Description : Provides Parsing benchmarks
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides a set of benchmarks to determine the performance of the Parsers the library exposes
-}

import           Criterion.Main
import qualified HSat.Parser                    as P
import qualified Language.CNF.Parse.ParseDIMACS as L

main :: IO ()
main = defaultMain [
  bgroup "CNF Parser Tests" $ map parseFunctions parseBench
  ]

{- Takes a description and a FilePath, and parses the file through the dimacs-parser
and the HSat parser (for CNF files
-}
parseFunctions :: (String,FilePath) -> Benchmark
parseFunctions (description,filePath) =
  bgroup description [
    bench "parse-dimacs" $ whnfIO (L.parseFile filePath),
    bench "HSat-parse" $ whnfIO $ P.fromFile P.parserInstances filePath
    ]

parseBench :: [(String,FilePath)]
parseBench = [
  ("Parsing a single small file","tests-src/Files/test1-good.cnf")
  ]
