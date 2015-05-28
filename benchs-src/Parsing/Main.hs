{-|
Module      : Parsing.Main
Description : The entry to the Parsing tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides Parsing benchmarks
-}

import           Criterion.Main
import qualified HSat.Parser as P
import qualified Language.CNF.Parse.ParseDIMACS as L

main :: IO ()
main = defaultMain [
  bgroup description1 [
     bench "parse-dimacs" $ whnfIO (L.parseFile filePath),
     bench "HSat parse" $ whnfIO (P.runReadFile (P.fromCNFFile filePath))
     ]
  ]

description1 :: String
description1 =
  "Parsing a single file"

filePath :: String
filePath = "tests-src/Files/test1-good.cnf"
