{-|
Module      : TestPrint.Problen.BSP.CNF.Builder
Description : The CNFBuilder PrintTest Node
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for CNFBuilder Test Printing
-}

module TestPrint.Problem.BSP.CNF.Builder (
  printer
  ) where

import           Control.Monad (liftM)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.CNF.Builder.Internal
import           HSat.Problem.BSP.Common
import           TestUtils

name :: String
name = "Builder"

printer :: TestTree
printer =
  testGroup name [
    printCNFBuilderArbitrary,
    printCNFBuilderErrorArbitrary
    ]

printCNFBuilderArbitrary :: TestTree
printCNFBuilderArbitrary =
  printTest "CNFBuilder" (
    generate arbitrary :: IO CNFBuilder)

printCNFBuilderErrorArbitrary :: TestTree
printCNFBuilderErrorArbitrary =
  printTest "CNFBuilderError" (
    generate arbitrary :: IO CNFBuilderError)
