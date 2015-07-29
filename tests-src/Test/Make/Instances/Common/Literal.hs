{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : Test.Make.Instances.Common.Literal
Description : Tests the Literal instances for Make
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Make Literal modules
-}

module Test.Make.Instances.Common.Literal (
  tests        ,-- :: TestTree
  arbLiteralSet,-- :: Int -> Gen LiteralSet
  ) where

import           Control.Monad.Catch
import           Control.Monad.State
import qualified Data.Map                           as M
import           Data.Set (Set)
import qualified Data.Set                           as S
import           HSat.Make.Config.Class
import           HSat.Make.Instances.Common.Literal
import           HSat.Problem.Instances.Common
import           HSat.Solution.Instances.CNF
import           Prelude                            hiding (lookup)
import           Test.Problem.Instances.Common.Sign ()
import           Test.Solution.Instances.CNF        (genBoolSolution)
import           TestUtils

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

arbLiteralSet      :: Int -> Gen LiteralSet
arbLiteralSet size = do
  --Choose random number, make sure it is at least 1
  maxVar            <- ((1+) . toEnum ) <$> choose (0,size)
  varsAppearTwice   <- arbitrary
  let varList       = map mkVariable [1..maxVar]
  --How many Variable's will be in 'varsThatCanAppear'
  noVarsToAppear    <- choose (0,maxVar-1)
  --Either no true values have been generated, or a number have been
  numbGeneratedTrue <- oneof [
    return 0,
    choose (0,noVarsToAppear-1)
    ]
  varsCanAppear     <- generateSet (S.fromList varList) noVarsToAppear
  solution          <- genBoolSolution maxVar size
  return $ LiteralSet varsCanAppear solution numbGeneratedTrue maxVar varsAppearTwice

mkLiteralSet1 :: TestTree
mkLiteralSet1 =
  testCase "mkLiteralSet 0" $ do
    gotten       <- mkLiteralSet 0 True
    let expected = LiteralSet S.empty emptySolution 0 0 True
    gotten       @=? expected

mkLiteralSet2 :: TestTree
mkLiteralSet2 =
  testProperty "mkLiteralSet non-zero" $ monadicIO $ do
    maxVar         <- pick $ choose (1,100)
    vAppearTwice   <- pick arbitrary
    LiteralSet{..} <- run $ mkLiteralSet maxVar vAppearTwice
    let vars          = map mkVariable [1..maxVar]
        exptdVars     = S.fromList vars
        gottenTrueSet = map fst . M.toList . solution $ getTrueSet
    
    stop (
      getVarsThatCanAppear === exptdVars    .&&.
      getMaximumVariable   === maxVar       .&&.
      gottenTrueSet        === vars         .&&.
      getHasGeneratedTrue  === 0            .&&.
      getVarsAppearTwice   === vAppearTwice
      )

generateNonFullVAppear   :: Int -> Gen LiteralSet
generateNonFullVAppear i = do
  maxVar        <- (1+) <$> choose (0,toEnum i)
  solution      <- genBoolSolution maxVar i
  removal       <- choose (1, maxVar - 1)
  varsToAppear  <- generateSet (S.fromList . map mkVariable $ [1.. maxVar]) removal
  generatedTrue <- choose (0,removal)
  return $ LiteralSet varsToAppear solution generatedTrue maxVar False

generateSet            :: (Ord a) => Set a -> Word -> Gen (Set a)
generateSet set remove =
  if set == S.empty || remove == 0 then
    return set else do
      index    <- choose (0, S.size set - 1)
      let set' = S.delete (S.elemAt index set) set
      generateSet set' (remove - 1)

resetTest1 :: TestTree
resetTest1 =
  testProperty "reset has correct vales" $ monadicIO $ do
    literalSet@LiteralSet{..} <- pick (sized generateNonFullVAppear)
    stop =<< run (
      catch (
         do
           (_,literalSet') <- runMake reset literalSet
           let exptdLiteralSet = literalSet {
                 getVarsThatCanAppear = S.fromList . map mkVariable $
                                        [1..getMaximumVariable],
                 getHasGeneratedTrue  = 0
                 }
           return $ exptdLiteralSet === literalSet'
         )
      (\exception ->
        let _ = exception :: MakeException
        in return $ counterexample ("Unexpected exception thrown: " ++ show exception) False
      )
      )

runMake :: LiteralMake monad result -> LiteralSet -> monad (result,LiteralSet)
runMake = runStateT

genericRunOutOfVars          :: LiteralMake IO Literal ->
                                TestTree
genericRunOutOfVars function =
  testProperty "Run out of Variables throws correct error" $ monadicIO $ do
    oldLitSet <- do
      ls <- pick arbitrary
      return $ ls {
        getVarsThatCanAppear = S.empty
        }
    stop =<< run (
      catch (
         do
           _ <- runMake function oldLitSet
           return $ counterexample "Expected error to be thrown" False
         )
      (\exception ->
        let _ = exception :: LiteralMakeError
        in return $ case exception of
            NoVariables -> property True
            _           -> counterexample "Incorrect error thrown" False
      )
      )

getRandomLiteralTest1 :: TestTree
getRandomLiteralTest1 =
  testProperty "getRandomLiteral has correct effects" $ monadicIO $ do
    literalSet <- pick arbitrary
    stop =<< run (
      catch (
         do
           (literal,literalSet') <- runMake getRandomLiteral literalSet
           return $ if getHasGeneratedTrue literalSet' > 0 then
             checkTrue literal literalSet literalSet' else
             checkGeneral literal literalSet literalSet'
      )
      (\exception ->
        let _ = exception :: MakeException
        in return $ counterexample "Incorrect error thrown" False
      )
      )
  where
    checkGeneral                                :: Literal -> LiteralSet -> LiteralSet -> Property
    checkGeneral literal literalSet literalSet' =
      let exptdCanAppear  = (
            if getVarsAppearTwice literalSet then
              id else S.delete (getVariable literal)
            ) (getVarsThatCanAppear literalSet)
          gottenCanAppear = getVarsThatCanAppear literalSet'
      in exptdCanAppear === gottenCanAppear

getTrueLiteralTest1 :: TestTree
getTrueLiteralTest1 =
  testProperty "getTrueLiteral has correct effects" $ monadicIO $ do
    literalSet <- pick arbitrary
    stop =<< run (
      catch (
         do
           (literal,literalSet') <- runMake getTrueLiteral literalSet
           return $ checkTrue literal literalSet literalSet'
           )
      (\exception ->
        let _ = exception :: MakeException
        in return $ counterexample ("Unexpected exception thrown: " ++ show exception) False
      )
      )

checkTrue                                            :: Literal -> LiteralSet -> LiteralSet -> Property
checkTrue literal oldLs@LiteralSet{..} newLiteralSet =
  let exptdLiteralSet = oldLs {
        getVarsThatCanAppear = if getVarsAppearTwice then
                               getVarsThatCanAppear else
                               S.delete (getVariable literal) getVarsThatCanAppear,
        getHasGeneratedTrue  = getHasGeneratedTrue + 1
        }
      sameSign = case lookup (getVariable literal) getTrueSet of
        Nothing -> counterexample "No Variable with that index in BoolSolution" False
        Just s' -> s' === getSign literal
  in exptdLiteralSet === newLiteralSet .&&. sameSign
