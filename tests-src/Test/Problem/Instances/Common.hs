{-|
Module      : Test.Problem.Instances.Common
Description : The Clauses data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The node within the test tree containing the tests of the children of the
'Common'
module
-}

module Test.Problem.Instances.Common (
  tests
  ) where

import qualified Test.Problem.Instances.Common.Clause as Clause
import qualified Test.Problem.Instances.Common.Clauses as Clauses
import qualified Test.Problem.Instances.Common.Literal as Literal
import qualified Test.Problem.Instances.Common.Sign as Sign
import qualified Test.Problem.Instances.Common.Variable as Variable
import           TestUtils

name :: String
name = "Common"

tests :: TestTree
tests =
  testGroup name [
    Clauses.tests ,
    Clause.tests  ,
    Literal.tests ,
    Variable.tests,
    Sign.tests
    ]
