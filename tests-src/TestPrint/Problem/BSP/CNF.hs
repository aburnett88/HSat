{-|
Module      : TestPrint.Problen.BSP.CNF
Description : The CNF type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the CNF type's Printer Tests
-}

module TestPrint.Problem.BSP.CNF (
  printer, -- TestTree
  cnfList  -- :: [CNF]
  ) where

import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.Common
import           TestPrint
import qualified TestPrint.Problem.BSP.CNF.Builder as CNFBuilder

name :: String
name = "CNF"

printer :: TestTree
printer =
  testGroup name [
    printList "CNF" cnfList,
    CNFBuilder.printer
    ]

cnfList :: [CNF]
cnfList = map (mkCNFFromClauses . mkClausesFromIntegers) [
  [],
  [[-1,-2,-3],[],[1,2,3]],
  [
    [-53,-345,234,237],
    [-24,24,675,1346],
    [2467,860,-1,2]
    ]
  ]

  
