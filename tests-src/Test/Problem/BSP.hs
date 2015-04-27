module Test.Problem.BSP (
  tests
  ) where

import TestUtils
import qualified Test.Problem.BSP.CNF as CNF
import qualified Test.Problem.BSP.Common as Common
import qualified Test.Problem.BSP.Internal as Internal
import HSat.Problem.BSP.Internal
import HSat.Problem.BSP

name :: String
name = "BSP"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    CNF.tests,
    Common.tests,
    testGroup "bool" [
      boolTest1
      ],
    testGroup "var" [
      varTest1
      ],
    testGroup "not'" [
      not'Test1
      ],
    testGroup "&&!" [
      andTest1
      ],
    testGroup "||!" [
      orTest1
      ],
    testGroup "==>!" [
      ifTest1
      ],
    testGroup "<==>!" [
      ifOnlyIfTest1
      ]
    ]

boolTest1 :: TestTree
boolTest1 =
  testProperty "bool b = (Bool' b)" $ property
  (\b ->
    let expected = ( Bool' b)
        gotten   = bool b
    in  expected === gotten
  )

varTest1 :: TestTree
varTest1 =
  testProperty "var v = (Variable' v)" $ property
  (\v ->
    let exptd = Variable' v
        gotten = var v
    in  exptd === gotten
  )

not'Test1 :: TestTree
not'Test1 =
  testProperty "not' bsp = (Not bsp)" $ property
  (\bsp ->
    let exptd = Not bsp
        gotten = not' bsp
    in  exptd === gotten
  )

andTest1 :: TestTree
andTest1 =
  testProperty "l &&! r = And l r" $ property
  (\(l,r) ->
    let exptd =  And l r
        gotten = l &&! r
    in exptd === gotten
  )

orTest1 :: TestTree
orTest1 =
  testProperty "l ||! r = Or l r" $ property
  (\(l,r) ->
    let exptd =  Or l r
        gotten = l ||! r
    in exptd === gotten
  )

ifTest1 :: TestTree
ifTest1 =
  testProperty "l ==>! r = If l r" $ property
  (\(l,r) ->
    let exptd =  If l r
        gotten = l ==>! r
    in exptd === gotten
  )

ifOnlyIfTest1 :: TestTree
ifOnlyIfTest1 =
  testProperty "l <==>! r = And l r" $ property
  (\(l,r) ->
    let exptd =  IfOnlyIf l r
        gotten = l <==>! r
    in exptd === gotten
  )






