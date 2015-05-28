{-|
Module      : Parsing.Main
Description : The entry to the Parsing tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides Parsing benchmarks
-}

import Criterion.Main
import qualified Language.CNF.Parse.ParseDIMACS as L
import qualified HSat.Parser as P

main :: IO ()
main = defaultMain [
  bgroup description1 [
     bench "parse-dimacs" $ whnf L.parseFile filePath,
     bench "HSat parse" $ whnf P.runReadFile (P.fromCNFFile "")
     ]
  ]

description1 :: String
description1 =
  "Parsing a single file"

filePath :: String
filePath = "tests-src/Files/test1-good.cnf"
