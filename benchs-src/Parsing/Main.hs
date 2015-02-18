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

main :: IO ()
main = defaultMain [
  bgroup "Parsing 1000 small files" []
  ]
