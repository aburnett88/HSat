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
  arbitrary = sized $ generateBSP (
    [Or,And,If,IfOnlyIf],
    [Not],
    [liftA Variable' arbitrary,
     liftA Bool' arbitrary]
    )
  shrink _ = []

generateBSP :: ([BSP -> BSP -> BSP],[BSP -> BSP],[Gen BSP]) -> Int -> Gen BSP
generateBSP tripple@(binary_constructors,unary_constructors,genericBSP) i
  | i < 1 = oneof genericBSP
  | otherwise =
    let total_size = length binary_constructors + length unary_constructors
    in case total_size of
     0 -> error "genericBSP: Not enough choices"
     _ -> do
       index <- choose (0,total_size - 1)
       case compare index (length binary_constructors) of
        LT -> do
          iLeft <- reduced
          iRight <- reduced
          l <- generateBSP tripple iLeft 
          r <- generateBSP tripple iRight
          return $ (binary_constructors !! index) l r
        _ -> do
          newI <- reduced
          bsp' <- generateBSP tripple newI
          return $ (unary_constructors !! (index - length binary_constructors)) bsp'
  where
    reduced = choose (0,round . log $ i')
    i' = fromIntegral i :: Double
    
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
  testProperty "Test NNF returns isNNf=True" $
  forAll
  (sized $ generateBSP (
      [Or,And],
      [Not],
      [
        liftA Variable' arbitrary,
        liftA Bool' arbitrary
        ]))
  (\bsp -> (isNNF . nnf $ bsp))
  

removeIfTest1 :: TestTree
removeIfTest1 =
  testProperty "Test removeIf returns noIfs==True" $
  forAll
  (sized $ generateBSP (
      [Or,And,If],
      [Not],
      [
        liftA Variable' arbitrary,
        liftA Bool' arbitrary
        ]))
  (\bsp ->
    noIfs $ removeIf bsp)

removeIffsTest1 :: TestTree
removeIffsTest1 =
  testProperty "Test removeIffs returns noIffs=True" $ property
  (\bsp ->
    noIffs $ removeIff bsp)

isFTests :: String -> (BSP -> Bool) -> Bool -> [BSP] -> [TestTree]
isFTests testname f true ls =
  isNNFTests' 1 ls
  where
    isNNFTests' :: Int -> [BSP] -> [TestTree]
    isNNFTests' _ [] = []
    isNNFTests' i (bsp:bsps) =
      (testCase (testname ++ " testcase example " ++ show i) $
      assertBool str (if true then f bsp else not $ f bsp)) :
      isNNFTests' (i+1) bsps
      where
        str = "Failure. Should have returned " ++ (show true) ++ (show bsp)


isNNFTests :: [TestTree]
isNNFTests =
  isFTests "isNNF" isNNF True [
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
  isFTests "isNotNNF" isNNF False [
    ex1,ex2,ex3,ex4]
  where
    ex1 = Not (Or (Variable' $ mkVariable 11) (Bool' True))
    ex2 = Not (Not (Bool' False))
    ex3 = If (Bool' True) (Bool' False)
    ex4 = IfOnlyIf (Bool' True) (Bool' False)

noIfsTests :: [TestTree]
noIfsTests =
  isFTests "noIfs" noIfs True [
    ex1,ex2,ex3,ex4,ex5]
  where
    ex1 = Bool' True
    ex2 = Variable' $ mkVariable 10
    ex3 = Not ex2
    ex4 = And ex2 ex3
    ex5 = Or ex4 ex2

notNoIfsTests :: [TestTree]
notNoIfsTests =
  isFTests "isNotIfs" noIfs False [
    ex1,ex2]
  where
    ex1 = If (Variable' $ mkVariable 9) (Bool' True)
    ex2 = IfOnlyIf (Variable' $ mkVariable 8) (Variable' $ mkVariable 7)

noIffsTests :: [TestTree]
noIffsTests =
  isFTests "noIffs" noIffs True [
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
  isFTests "isNotNoIffs" noIffs False [
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
