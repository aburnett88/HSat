{-|
Module      : Test.Data.BSP.CNF.Parser
Description : The tests for the CNF Parser type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the tests for the CNF Parser
-}

module Test.Data.BSP.CNF.Parser (
  tests
  ) where

import TestUtils
import HSat.Data.BSP.CNF.Parser as P
import HSat.Data.BSP.CNF as C
import System.Directory
import HSat.Data.BSP.CNF.Builder
import qualified HSat.Data.BSP.Common.Literal as L

name :: String
name = "HSat.Data.BSP.CNF.Parser"

tests :: TestTree
tests =
  testGroup name [
    testCase "Parse a simple CNF file" test1,
    testCase "Parse a spacey CNF file" test2,
    testCase "Parse a commenty CNF file" test3,
    testCase "Parse a commenty and spacey CNF file" test4,
    testCase "parse a multi-line clause CNF file" test5,
    testCase "parse a multi-line and commenty CNF file" test6,
    testCase "parse an emtpy CNF file" test7,
    testCase "parse a CNF File, weirdness at the end" test8,
    --files that SHALL NOT PASS
    testCase "don't parse file with no zero at the end" test9,
    testCase "Don't parse a zero'ing CNF file property" test10,
    testCase "don't parse a negatie CNF file" test11,
    --Tests that should just return nothing
    testCase "too many clauses == Nothing" test12,
    testCase "too few clauses == Nothing" test13,
    testCase "var outside range == Nothing" test14,
    testCase "too many clauess" test15,
    testCase "too few clauses" test16,
    testCase "var outside range" test17,
    testCase "Should fail as return at start of file" test18
    ]

failure :: String -> Either Error CNF -> Either Error CNF -> String
failure msg (Left c) (Right e) =
  "Error parsing " ++ msg ++
  "Error generated: " ++ (show c)
failure msg (Right l) (Right e) =
  "Error parsing "++ msg ++
  "Expected: " ++ (show e) ++ "\n But gotten: " ++
  (show l)
failure s a b =
  "Error parsing " ++ s ++
  "\nGotten " ++ (show a) ++ "\nExpected" ++ (show b)

folder :: String -> String
folder s = "tests-src/Files/" ++ s

loc1,loc2,loc3,loc4,loc5,loc6,loc7,loc8,loc9,loc10,loc11,loc12,loc13,loc14 :: String
loc1 = "test1-good.cnf"
loc2 = "test2-good.cnf"
loc3 = "test3-good.cnf"
loc4 = "test4-good.cnf"
loc5 = "test5-good.cnf"
loc6 = "test6-good.cnf"
loc7 = "test7-good.cnf"
loc8 = "test8-good.cnf"
loc9 = "test9-bad.cnf"
loc10 = "test10-bad.cnf"
loc11 = "test11-bad.cnf"
loc12 = "test12-bad.cnf"
loc13 = "test13-bad.cnf"
loc14 = "test14-bad.cnf"
loc15 = "test15-bad.cnf"

  
test1 :: Assertion
test1 = do
  let loc = folder loc1
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

example :: Either Error C.CNF
example = return . C.fromInts $ [[1,-3],[2,3,-1]]

test2 :: Assertion
test2 = do
  let loc = folder loc2
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

test3 :: Assertion
test3 = do
  let loc = folder loc3
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

test4 :: Assertion
test4 = do
  let loc = folder loc4
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

test5 :: Assertion
test5 = do
  let loc = folder loc5
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

test6 :: Assertion
test6 = do
  let loc = folder loc6
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

test7 :: Assertion
test7 = do
  let loc = folder loc7
      example' = return . C.fromInts $ []
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example')
    (cnf == example')

test8 :: Assertion
test8 = do
  let loc = folder loc8
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

test9 :: Assertion
test9 = do
  let loc = folder loc9
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example)
    (cnf == example)

test10 :: Assertion
test10 = do
  let loc = folder loc10
      example' = Left $ IncorrectClauseNo 2 1
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example')
    (cnf == example')

test11 :: Assertion
test11 = do
  let loc = folder loc11
  cnf <- P.parseCNF loc
  assertBool (
             parseErr loc cnf)
    (isParseErr cnf)

parseErr :: String -> Either Error CNF -> String
parseErr loc (Right r) =
  "Should be a parser Error. Instead got " ++ (show r)
parseErr loc (Left err) =
  "Massive error. Parser error expected. Problem with " ++
  (show err)

isParseErr :: Either Error CNF -> Bool
isParseErr (Left (OtherError _)) = True
isParseErr _ = False

test12 :: Assertion
test12 = do
  let loc = folder loc12
  cnf <- P.parseMaybe loc
  assertBool "Isn't a maybe" (cnf == Nothing)

test13 :: Assertion
test13 = do
  let loc = folder loc13
  cnf <- P.parseMaybe loc
  assertBool "Isn't a Maybe" (cnf == Nothing)

test14 :: Assertion
test14 = do
  let loc = folder loc14
  cnf <- P.parseMaybe loc
  assertBool "Isn't a maybe" (cnf == Nothing)

test15 :: Assertion
test15 = do
  let loc = folder loc12
      example' = Left $ IncorrectClauseNo 2 3
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example')
    (cnf == example')

test16 :: Assertion
test16 = do
  let loc = folder loc13
      example' = Left $ IncorrectClauseNo 2 1
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example')
    (cnf == example')

test17 :: Assertion
test17 = do
  let loc = folder loc14
      example' = Left $ LitOutsideRange (L.fromInt 6) 3
  cnf <- P.parseCNF loc
  assertBool (
             failure loc cnf example')
    (cnf == example')

test18 :: Assertion
test18 = do
  let loc = folder loc15
  cnf <- P.parseCNF loc
  assertBool (
             parseErr loc cnf)
    (isParseErr cnf)
