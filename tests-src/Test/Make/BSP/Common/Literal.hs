module Test.Make.BSP.Common.Literal (
  tests,
  arbLiteralSet
  ) where

import TestUtils
import HSat.Make.BSP.Common.Literal
import HSat.Problem.BSP.Common
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set (Set)
import Test.Problem.BSP.Common.Sign ()
import Control.Applicative

name :: String
name = "Literal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkLiteralSet" [
       mkLiteralSet1,
       mkLiteralSet2
       ],
    testGroup "reset" [
      resetTest1
      ],
    testGroup "getTrueLiteral" [
      getTrueLiteralTest1,
      genericRunOutOfVars getTrueLiteral
      ],
    testGroup "getRandomLiteral" [
      getRandomLiteralTest1,
      genericRunOutOfVars getRandomLiteral
      ]
    ]

instance Arbitrary LiteralSet where
  arbitrary = sized arbLiteralSet

arbLiteralSet :: Int -> Gen LiteralSet
arbLiteralSet size = do
  --Choose random number, make sure it is atleast 1
  maxVar <- ((1+) . toEnum ) <$> choose (0,size)
  varsAppearTwice <- arbitrary
  let varList = map mkVariable [1..maxVar]
  varMapping <- M.fromList `liftA` mapM (
    \v -> ((,)v) `liftA` arbitrary) varList
  --How many Variable's will be in 'varsThatCanAppear'
  noVarsToAppear <- choose (0,maxVar-1)
  --Either no true values have been generated, or a number have been
  numbGeneratedTrue <- oneof [
    return 0,
    choose (0,noVarsToAppear-1)
    ]
  varsCanAppear <- generateSet (S.fromList varList) noVarsToAppear
  return $ LiteralSet varsCanAppear varMapping numbGeneratedTrue maxVar varsAppearTwice

f :: (MonadRandom m) => LiteralMake m a -> LiteralSet ->
     m (Either LiteralMakeError (a,LiteralSet))
f func initial = runEitherT (runStateT func initial)
  

mkLiteralSet1 :: TestTree
mkLiteralSet1 =
  testCase "mkLiteralSet 0" $ do
    gotten <- mkLiteralSet 0 True
    let expected = LiteralSet S.empty M.empty 0 0 True
    assert ( gotten == expected)

mkLiteralSet2 :: TestTree
mkLiteralSet2 =
  testProperty "mkLiteralSet non-zero" $ ioProperty $ do
    (maxVar,vAppearTwice) <- generate $ do
      maxVar' <- choose (1,100)
      vAppearTwice' <- arbitrary
      return (maxVar',vAppearTwice')
    gotten <- mkLiteralSet maxVar vAppearTwice
    let vars = map mkVariable [1..maxVar]
        exptdVars = S.fromList vars
        exptdTrueSet = vars
        gottenTrueSet = map fst . M.toList $ getTrueSet gotten
        gottenVars = getVarsThatCanAppear gotten
        gottenMaxVar = getMaximumVariable gotten
        gottenHasGeneratedTrue = getHasGeneratedTrue gotten
        gottenVarsAppearTwice = getVarsAppearTwice gotten
    return $ property $ (exptdTrueSet === gottenTrueSet) .&&.
             (gottenVars === exptdVars) .&&.
             (gottenMaxVar === maxVar) .&&.
             (gottenHasGeneratedTrue === 0) .&&.
             (gottenVarsAppearTwice === vAppearTwice)

generateNonFullVAppear :: Gen LiteralSet
generateNonFullVAppear = do
  v <- choose (1,100)
  m <- M.fromList `liftM` mapM (\var -> do
                                     s <- arbitrary
                                     return (mkVariable var,s)) [1..v]
  removal <- choose (1,v-1)
  s <- generateSet (S.fromList . map mkVariable $ [1..v]) removal
  l <- choose (0,removal)
 -- if toEnum $ S.size s == (v-removal) then
  return (LiteralSet s m l v False) --else
  --  return (LiteralSet s m l v False)

generateSet :: (Ord a) => Set a -> Word -> Gen (Set a)
generateSet s n =
  if s==S.empty || n == 0 then
    return s else do
      index <- choose (0, S.size s-1)
      let s' = S.delete (S.elemAt index s) s
      generateSet s' (n-1)

resetTest1 :: TestTree
resetTest1 =
  testProperty "reset has correct vals" $ ioProperty $ do
    literalset <- generate generateNonFullVAppear
    result <- f reset literalset
    case result of
      Left _ -> return $ property False
      Right (_,ls) -> do
        let exptd = literalset {
              getVarsThatCanAppear = S.fromList . map mkVariable $
                                     [1..(getMaximumVariable ls)],
              getHasGeneratedTrue  = 0
              }
        return $ property $ exptd === ls

genericRunOutOfVars :: LiteralMake IO Literal ->
                       TestTree
genericRunOutOfVars function =
  testCase "Run out of Variables throws correct error" $ do
    oldLitSet <- generate arbitrary
    let ls = oldLitSet {
          getVarsThatCanAppear = S.empty
              }
    result <- f function ls
    case result of
      Left NoVariables -> assert True
      _ -> assert False

getRandomLiteralTest1 :: TestTree
getRandomLiteralTest1 =
  testProperty "getRandomLiteral has correct semantics" $ ioProperty $ do
    ls <- generate arbitrary
    let hasGen = getHasGeneratedTrue ls
    result <- f getRandomLiteral ls
    return $ case result of
      Left _ -> property False
      Right (l,newLs) -> 
        let newHasGen = getHasGeneratedTrue newLs
        in if newHasGen > hasGen then
          checkTrue l ls newLs else
          checkGeneral l ls newLs

checkGeneral :: Literal -> LiteralSet -> LiteralSet -> Property
checkGeneral l oldLs newLs =
  let vAppearTwice = getVarsAppearTwice oldLs
      oldSet = getVarsThatCanAppear oldLs
      exptdCanAppear = if vAppearTwice then
                         oldSet else
                         S.delete (getVariable l) oldSet
  in property $ exptdCanAppear == getVarsThatCanAppear newLs

getTrueLiteralTest1 :: TestTree
getTrueLiteralTest1 =
  testProperty "getTrueLiteral has correct semantics" $ ioProperty $ do
    ls <- generate arbitrary
    result <- f getTrueLiteral ls
    return $ case result of
      Left e -> counterexample (
        "Threw an unexepcted Left: " ++
        show e ++
        "\nInitial LiteralSet Used: " ++
        show ls) False
      Right (l,ls') -> checkTrue l ls ls'

checkTrue :: Literal -> LiteralSet -> LiteralSet -> Property
checkTrue l oldLs newLs =
  let vAppearTwice = getVarsAppearTwice oldLs
      oldSet = getVarsThatCanAppear oldLs
      exptdVCanAppear = if vAppearTwice then
                          oldSet else
                          S.delete (getVariable l) oldSet
      mapping = getTrueSet oldLs
      sameSign = case lookup (getVariable l) mapping of
        Nothing -> False
        Just s' -> s' == getSign l
      exptdNumb = getHasGeneratedTrue oldLs + 1
      exptdLs = oldLs {
        getVarsThatCanAppear = exptdVCanAppear,
        getHasGeneratedTrue = exptdNumb
                              }
   in counterexample ("Non equal on literal" ++ show l) $
      (exptdLs === newLs) .&&. (sameSign === True)
