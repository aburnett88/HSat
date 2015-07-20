module Test.Make.Common (
  tests
  ) where

import TestUtils
import HSat.Make.Common

name :: String
name = "Common"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkBounds" [
       checkBoundsTest1 ("Char",sized genMkBounds :: Gen (Bounds Char,Char,Char)),
       checkBoundsTest1 ("Int",sized genMkBounds :: Gen (Bounds Int,Int,Int)),
       checkBoundsTest1 ("Word",sized genMkBounds :: Gen (Bounds Word,Word,Word)),
       checkBoundsTest2 ("Char",sized genMkBounds :: Gen (Bounds Char,Char,Char)),
       checkBoundsTest2 ("Int",sized genMkBounds :: Gen (Bounds Int,Int,Int)),
       checkBoundsTest2 ("Word",sized genMkBounds :: Gen (Bounds Word,Word,Word))
       ],
    testGroup "mkExact" [
       checkMaxExactTest ("mkExact","Char",(sized genExact :: Gen (Bounds Char,Char))),
       checkMaxExactTest ("mkExact","Int",(sized genExact :: Gen (Bounds Int,Int))),
       checkMaxExactTest ("mkExact","Word",(sized genExact :: Gen (Bounds Word,Word))),
       checkMinExactTest ("mkExact","Char",(sized genExact :: Gen (Bounds Char,Char))),
       checkMinExactTest ("mkExact","Int",(sized genExact :: Gen (Bounds Int,Int))),
       checkMinExactTest ("mkExact","Word",(sized genExact :: Gen (Bounds Word,Word)))
       ],
    testGroup "mkMinimum" [
       checkMaxBoundsTest ("mkMinimum","Char",(fst <$> sized genMinimum :: Gen (Bounds Char))),
       checkMaxBoundsTest ("mkMinimum","Int",(fst <$> sized genMinimum :: Gen (Bounds Int))),
       checkMaxBoundsTest ("mkMinimum","Word",(fst <$> sized genMinimum :: Gen (Bounds Word))),
       checkMinExactTest ("mkMinimum","Char",(sized genMinimum :: Gen (Bounds Char, Char))),
       checkMinExactTest ("mkMinimum","Int",(sized genMinimum :: Gen (Bounds Int, Int))),
       checkMinExactTest ("mkMinimum","Word",(sized genMinimum :: Gen (Bounds Word, Word)))
       ],
    testGroup "mkMaximum" [
       checkMinBoundsTest ("mkMaximum","Char",(fst <$> sized genMaximum :: Gen (Bounds Char))),
       checkMinBoundsTest ("mkMaximum","Int",(fst <$> sized genMaximum :: Gen (Bounds Int))),
       checkMinBoundsTest ("mkMaximum","Word",(fst <$> sized genMaximum :: Gen (Bounds Word))),
       checkMaxExactTest ("mkMaximum","Char",(sized genMaximum :: Gen (Bounds Char,Char))),
       checkMaxExactTest ("mkMaximum","Int",(sized genMaximum :: Gen (Bounds Int,Int))),
       checkMaxExactTest ("mkMaximum","Word",(sized genMaximum :: Gen (Bounds Word,Word)))
       ],
    testGroup "mkNoBounds" [
       checkMinBoundsTest ("mkNoBounds","Char",(sized genNoBounds :: Gen (Bounds Char))),
       checkMinBoundsTest ("mkNoBounds","Int",(sized genNoBounds :: Gen (Bounds Int))),
       checkMinBoundsTest ("mkNoBounds","Word",(sized genNoBounds :: Gen (Bounds Word))),
       checkMaxBoundsTest ("mkNoBounds","Char",(sized genNoBounds :: Gen (Bounds Char))),
       checkMaxBoundsTest ("mkNoBounds","Int",(sized genNoBounds :: Gen (Bounds Int))),
       checkMaxBoundsTest ("mkNoBounds","Word",(sized genNoBounds :: Gen (Bounds Word)))
       ],
    testGroup "getLesser" [
       getLesserTest1 ("Char", arbitrary :: Gen (Bounds Char)),
       getLesserTest1 ("Int", arbitrary :: Gen (Bounds Int)),
       getLesserTest1 ("Word", arbitrary :: Gen (Bounds Word))
       ],
    testGroup "getGreater" [
       getGreaterTest1 ("Char", arbitrary :: Gen (Bounds Char)),
       getGreaterTest1 ("Int", arbitrary :: Gen (Bounds Int)),
       getGreaterTest1 ("Word", arbitrary :: Gen (Bounds Word))
       ],
    testGroup "mkPosDouble" [
       mkPosDoubleTest1,
       mkPosDoubleTest2
       ],
    testGroup "getDouble" [
       getDoubleTest1
       ]
    ]

genMkBounds :: (Arbitrary a, Bounded a, Ord a) => Int -> Gen (Bounds a, a, a)
genMkBounds _ = do
  x <- arbitrary
  y <- arbitrary
  let x' = if x < y then x else y
      y' = if y >= x then y else x
  return $ (mkBounds x y, x', y')

checkBoundsTest1 :: (Ord a, Bounded a, Show a, Eq a) => (String, Gen (Bounds a, a, a)) -> TestTree
checkBoundsTest1 (str1,arb) =
  testProperty ("getLesser $ mkExact " `equiv` " fst with " ++ str1) $
  forAll
  arb
  (\(bounds,a,_) ->
    counterexample ("getLesser $ mkExact != fst ") ((getLesser $ bounds) === a)
  )

checkBoundsTest2 :: (Ord a, Bounded a, Show a, Eq a) => (String, Gen (Bounds a, a, a)) -> TestTree
checkBoundsTest2 (str1,arb) =
  testProperty ("getGreater $ mkExact " `equiv` " snd with " ++ str1) $
  forAll
  arb
  (\(bounds,_,b) ->
    counterexample ("getGreater $ mkExact != snd ") ((getGreater $ bounds) === b)
  )

checkMinExactTest :: (Ord a, Bounded a, Show a, Eq a) => (String,String, Gen (Bounds a,a)) -> TestTree
checkMinExactTest (str1, str2, arb) =
  testProperty ("getLesser $ " ++ str1  `equiv` " e with " ++ str2) $
  forAll
  arb
  (\(bounds,a) ->
    counterexample ("getLesser $ " ++ str1 ++ "!= e") ((getLesser $ bounds) === a)
  )

checkMaxExactTest :: (Ord a, Bounded a, Show a, Eq a) => (String,String, Gen (Bounds a,a)) -> TestTree
checkMaxExactTest (str1, str2, arb) =
  testProperty ("getGreater $ "  ++ str1 `equiv` " e with " ++ str2) $
  forAll
  arb
  (\(bounds,a) ->
    counterexample ("getGreater $ " ++ str1 ++ "!= e") ((getGreater $ bounds) === a)
  )

checkMaxBoundsTest :: (Ord a, Bounded a, Show a, Eq a) => (String, String, Gen (Bounds a)) -> TestTree
checkMaxBoundsTest (str1,str2,arb) =
  testProperty ("getGreater $ " ++ str1 `equiv` " maxBound with " ++ str2) $
  forAll
  arb
  (\b ->
    counterexample ("getGreater $ " ++ str1 ++ " != maxBound") ((getGreater $ b) === maxBound)
  )

genNoBounds :: (Ord a, Bounded a) => Int -> Gen (Bounds a)
genNoBounds _ = return mkNoBounds

genExact :: (Ord a, Bounded a, Arbitrary a) => Int -> Gen (Bounds a, a)
genExact _ =
  (\a -> (mkExact a,a)) <$> arbitrary

genMinimum :: (Ord a, Bounded a, Arbitrary a) => Int -> Gen (Bounds a,a)
genMinimum _ =
  (\a -> (mkMinimum a, a)) <$> arbitrary

genMaximum :: (Ord a, Bounded a, Arbitrary a) => Int -> Gen (Bounds a,a)
genMaximum _ =
  (\a -> (mkMaximum a, a)) <$> arbitrary

checkMinBoundsTest :: (Ord a, Bounded a, Show a, Eq a) => (String, String, Gen (Bounds a)) -> TestTree
checkMinBoundsTest (str1,str2,arb) =
  testProperty ("getLesser $ " ++ str1 `equiv` " minBound with " ++ str2) $
  forAll
  arb
  (\b ->
    counterexample ("getLesser $ " ++ str1 ++ " != minBound") ((getLesser $ b) === minBound)
  )

getLesserTest1 :: (Ord a, Bounded a, Show a) => (String, Gen (Bounds a)) -> TestTree
getLesserTest1 (str,arb) =
  testProperty ("getLesser <= getGreater with " ++ str) $
  forAll
  arb
  (\b ->
    counterexample "getGreater < getLesser" (getGreater b >= getLesser b)
  )
  
getGreaterTest1 :: (Ord a, Bounded a, Show a) => (String, Gen (Bounds a)) -> TestTree
getGreaterTest1 (str,arb) =
  testProperty ("getGreater >= getLesser with " ++ str) $
  forAll
  arb
  (\b ->
    counterexample "getGreater < getLesser" (getGreater b >= getLesser b)
  )

mkPosDoubleTest1 :: TestTree
mkPosDoubleTest1 =
  testProperty ("mkPosDouble . getDouble " `equiv` " id") $
  forAll
  (mkPosDouble <$> sized genPosDouble)
  (\d ->
    (mkPosDouble . getDouble $ d) === d
  )

mkPosDoubleTest2 :: TestTree
mkPosDoubleTest2 =
  testCase "mkPosDouble negative value throws runtime error" $
  forceError . mkPosDouble  $ -1.0

genPosDouble :: Int -> Gen Double
genPosDouble _ =
  (\d -> if d < 0.0 then abs d else d) <$> arbitrary

getDoubleTest1 :: TestTree
getDoubleTest1 =
  testProperty ("getDouble . mkPosDouble " `equiv` " id") $
  forAll
  (sized genPosDouble)
  (\posDouble ->
    (getDouble $ mkPosDouble posDouble) === posDouble
  )


