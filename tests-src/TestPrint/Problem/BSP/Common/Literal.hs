{-|
Module      : TestPrint.Problen.BSP.Common.Literal
Description : The Literal type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Literal type's Printer Tests
-}

module TestPrint.Problem.BSP.Common.Literal (
  printer -- :: TestTree
  ) where

import HSat.Problem.BSP.Common.Literal
import TestPrint

name :: String
name = "Literal"

printer :: TestTree
printer =
  testGroup name [
    printList "Positive Literals" $ map mkLiteralFromInteger lits,
    printList
     "Negative Literals" $
     map (mkLiteralFromInteger . negate) lits
    ]
  where
    lits :: [Integer]
    lits = [
      1,10,100,1000,10000,100000000000000000,
      1345,123456567,234565537686,134,6767,2689,379775,
      134556757777777,995696874834
      ]
