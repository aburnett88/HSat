{-|
Module      : Test.Problem.Instances.CNF.Writer
Description : Tests for the CNF Writer
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports the tests for the CNF Writer
-}

module Test.Problem.Instances.CNF.Writer (
  tests -- TestTree
  ) where

import Data.Attoparsec.Text
import HSat.Problem.Instances.CNF.Parser
import HSat.Problem.Instances.CNF.Writer
import Test.Problem.Instances.CNF.Internal ()
import TestUtils

name :: String
name = "Writer"

tests :: TestTree
tests =
  testGroup name [
    testGroup "toText" [
       toTextTest1
       ]
    ]

toTextTest1 :: TestTree
toTextTest1 =
  testProperty ("read . write " `equiv` " id") $ property
  (\cnf ->
    case parseOnly cnfParser $ toText cnf of
     Right (Right cnf') -> cnf' === cnf
     _ -> counterexample "error" False
  )
