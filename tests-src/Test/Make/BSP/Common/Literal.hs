module Test.Make.BSP.Common.Literal (
  tests
  ) where

import TestUtils
import HSat.Make.BSP.Common.Literal
import HSat.Problem.BSP.Common.Literal
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Set as S

name :: String
name = "Literal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "makeLiteral" [
       makeLiteralTest1,
       makeLiteralTest2,
       makeLiteralTest3,
       makeLiteralTest4,
       makeLiteralTest5
       ]
    ]

x :: (MonadRandom m) => LiteralStatusError m Literal -> LiteralMakeStatus -> m (Either LiteralMakeError (Literal,LiteralMakeStatus))
x lse lms = runEitherT (runStateT lse lms)

makeLiteralTest1 :: TestTree
makeLiteralTest1 =
  testCase "makeLiteral == generates correct literal" $ do
    let litMakeStatus = LiteralMakeStatus 12 litGenSet
        litGenSet     = mkVarSet 10
    lit <- x makeLiteral litMakeStatus
    assert $ checkLiteral lit litMakeStatus

checkLiteral :: Either a (Literal,LiteralMakeStatus) -> LiteralMakeStatus -> Bool
checkLiteral (Left _) _ = False
checkLiteral (Right (lit,(LiteralMakeStatus w lgen))) (LiteralMakeStatus w' lgen') =
  let checkCounter = w == (w'-1)
      sizeComparison = compare w' (getSize lgen')
      inOldSet = case lgen' of
        (NoSet _) -> True
        (VariableSet s _) -> S.member (getVariable lit) s
        (LiteralSet s _) -> S.member lit s
      notInNewSet = case lgen of
        (NoSet _) -> True
        (VariableSet s _) -> not $ S.member (getVariable lit) s
        (LiteralSet s _) -> not $ S.member lit s
  in checkCounter && (case sizeComparison of
        EQ -> notInNewSet && inOldSet
        LT -> False
        GT -> if inOldSet then
                notInNewSet else
                True
                )

makeLiteralTest2 :: TestTree
makeLiteralTest2 =
  testCase "makeLiteral == generates single correct literal" $ do
    let literal = mkLiteralFromInteger (-32)
        litMakeStatus = LiteralMakeStatus 1 litGenSet
        litGenSet =  LiteralSet (S.fromList . map mkLiteralFromInteger $ [-32]) 48
    lit <- x makeLiteral litMakeStatus
    case lit of
      Right (lit',_) -> assert $ (lit' == literal)
      _ -> assert $ False
    
makeLiteralTest3 :: TestTree
makeLiteralTest3 =
  testProperty "Generated litMakeStatus returns correct literal" $
  forAll
  (return $ LiteralMakeStatus 0 (mkNoSet 100))
  (\makeStatus -> ioProperty $ do
      literal <- x makeLiteral makeStatus
      return $ property $ checkLiteral literal makeStatus
      )

makeLiteralTest4 :: TestTree
makeLiteralTest4 =
  testProperty "makeLiteral litMakeStatus returns something within the lit set" $
  forAll
  (return $ LiteralMakeStatus 0 (mkNoSet 100))
  (\makeStatus -> ioProperty $ do
      literal <- x makeLiteral makeStatus
      return $ property $ checkLiteral literal makeStatus
      )

makeLiteralTest5 :: TestTree
makeLiteralTest5 =
  testProperty "makeLiteral (Generate litmakestatus that cannot be satisfied throws error" $
  forAll
  (return $ LiteralMakeStatus 0 (mkNoSet 100))
  (\makeStatus -> ioProperty $ do
      literal <- x makeLiteral makeStatus
      return $ case literal of
        Left err -> property True
        _ -> property False
        )
