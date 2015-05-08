module Test.Problem.BSP.Internal (
  tests
  ) where

import Control.Applicative
import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Internal
import Test.Problem.BSP.Common.Variable ()
import TestUtils
import TestUtils.Validate

tests :: TestTree
tests = testGroup name [
  bspTest1,
  testGroup "nnf" [
    nnfTest1
    ],
  testGroup "removeIf" [
    removeIfTest1
    ],
  testGroup "removeIff" [
    removeIffsTest1
    ],
  testGroup "isNNF" (
    isNNFTests ++ isNotNNFTests
    ),
  testGroup "noIfs" (
    noIfsTests ++ notNoIfsTests
    ),
  testGroup "noIffs" (
    noIffsTests ++ notNoIffsTests
    ),
  testGroup "permutations" (
    [
      permutationsTest1
      ] ++
    permutationsTestN examples
    )
  ]

name :: String
name = "Internal"

instance Arbitrary BSP where
  arbitrary =
    sized bsp'
    where
      reduce :: Int -> Gen Int
      reduce i = choose (0,round . log $ i')
        where
          i' = fromIntegral i :: Double
      bsp' :: Int -> Gen BSP
      bsp' i
        | i < 1 = oneof [
            liftA Bool' arbitrary,
            liftA Variable' arbitrary
            ]
        | otherwise = oneof [
            do
              i' <- reduce i
              n <- bsp' i'
              return $ Not n,
            do
              i1 <- reduce i
              i2 <- reduce i
              l <- bsp' i1
              r <- bsp' i2
              return $ And l r,
            do
              i1 <- reduce i
              i2 <- reduce i
              l <- bsp' i1
              r <- bsp' i2
              return $ Or l r,
            do
              i1 <- reduce i
              i2 <- reduce i
              l <- bsp' i1
              r <- bsp' i2
              return $ If l r,
            do
              i1 <- reduce i
              i2 <- reduce i
              l <- bsp' i1
              r <- bsp' i2
              return $ IfOnlyIf l r
              ]
  shrink _ = []


bspTest1 :: TestTree
bspTest1 =
  testProperty "Arbitrary created BSP's are valid" $ property bspTest
  where
    bspTest :: BSP -> Bool
    bspTest = validate
  
                  
instance Validate BSP where
  validate (Variable' v) = validate v
  validate (Not bsp) = validate bsp
  validate (Or l r) =
    validate l && validate r
  validate (And l r) =
    validate l && validate r
  validate (If l r) =
    validate l && validate r
  validate (IfOnlyIf l r) =
    validate l && validate r
  validate _ = True

nnfTest1 :: TestTree
nnfTest1 =
  testProperty "Test NNF returns isNNf=True" $ property
  (\bsp ->
    isNNF . nnf $ bsp)

removeIfTest1 :: TestTree
removeIfTest1 =
  testProperty "Test removeIf returns noIfs==True" $ property
  (\bsp ->
    noIfs $ removeIf bsp)

removeIffsTest1 :: TestTree
removeIffsTest1 =
  testProperty "Test removeIffs returns noIffs=True" $ property
  (\bsp ->
    noIffs $ removeIff bsp)

isFTests :: (BSP -> Bool) -> Bool -> [BSP] -> [TestTree]
isFTests f true ls =
  isNNFTests' 1 ls
  where
    isNNFTests' :: Int -> [BSP] -> [TestTree]
    isNNFTests' _ [] = []
    isNNFTests' i (bsp:bsps) =
      (testCase ("isNNF testcase example " ++ show i) $
      assertBool str (if true then f bsp else not $ f bsp)) :
      isNNFTests' (i+1) bsps
      where
        str = "Failure. Should have returned " ++ (show true)


isNNFTests :: [TestTree]
isNNFTests =
  isFTests isNNF True [
    ex1,ex2,ex3,ex4,ex5,ex6]
  where
    ex1 = Not (Bool' True)
    ex2 = Variable' $ mkVariable 12
    ex3 = Not (Bool' False)
    ex4 = Variable' $ mkVariable 15
    ex5 = And ex1 ex4
    ex6 = Or ex2 ex5

isNotNNFTests :: [TestTree]
isNotNNFTests =
  isFTests isNNF False [
    ex1,ex2,ex3,ex4]
  where
    ex1 = Not (Or (Variable' $ mkVariable 11) (Bool' True))
    ex2 = Not (Not (Bool' False))
    ex3 = If (Bool' True) (Bool' False)
    ex4 = IfOnlyIf (Bool' True) (Bool' False)

noIfsTests :: [TestTree]
noIfsTests =
  isFTests noIfs True [
    ex1,ex2,ex3,ex4,ex5]
  where
    ex1 = Bool' True
    ex2 = Variable' $ mkVariable 10
    ex3 = Not ex2
    ex4 = And ex2 ex3
    ex5 = Or ex4 ex2

notNoIfsTests :: [TestTree]
notNoIfsTests =
  isFTests noIfs False [
    ex1,ex2]
  where
    ex1 = If (Variable' $ mkVariable 9) (Bool' True)
    ex2 = IfOnlyIf (Variable' $ mkVariable 8) (Variable' $ mkVariable 7)

noIffsTests :: [TestTree]
noIffsTests =
  isFTests noIfs True [
    ex1,ex2,ex3,ex4,ex5,ex6]
  where
    ex1 = Bool' True
    ex2 = Variable' $ mkVariable 6
    ex3 = Not ex2
    ex4 = And ex2 ex3
    ex5 = Or ex4 ex2
    ex6 = If ex5 ex4

notNoIffsTests :: [TestTree]
notNoIffsTests =
  isFTests noIfs False [
    ex1]
  where
    ex1 = IfOnlyIf (Variable' $ mkVariable 5) (Variable' $ mkVariable 4)

permutationsTest1 :: TestTree
permutationsTest1 =
  testProperty "Test permutations return correct number" $ property
  (\bsp ->
    let exptd = numberPermutations $ bsp
        gotten = length . permutations $ bsp
    in exptd === gotten)

permutationsTestN :: [(BSP,[BSP])] -> [TestTree]
permutationsTestN bspss =
  permutationsTest' 1 bspss
  where
    permutationsTest' :: Int -> [(BSP,[BSP])] -> [TestTree]
    permutationsTest' _ [] = []
    permutationsTest' i ((bsp,bsps):rest) =
      (testCase ("Test permutations returns expected list on example " ++ show i) $
      assertBool "Failure. Lists not contain same elements" (
        listContainsSame'2' (permutations bsp) bsps)) : permutationsTest' (i+1) rest
      
examples :: [(BSP,[BSP])]
examples = [(ex1,ex1s)]
  where
    ex1 = a1d
    ex1s = [a1b,a1c,a1d,a1a]
    v1 = Variable' $ mkVariable 3
    v2 = Variable' $ mkVariable 2
    v3 = Variable' $ mkVariable 1
    o1a = Or v2 v3
    o1b = Or v3 v2
    a1a = And v1 o1a
    a1b = And o1a v1
    a1c = And v1 o1b
    a1d = And o1b v1
