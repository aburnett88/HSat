module Test.Make.BSP.Common.Clause (
  tests
  ) where

import TestUtils
import HSat.Make.BSP.Common.Literal
import HSat.Make.BSP.Common.Clause
import Control.Monad.Random
import Control.Monad.Trans.Either
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import HSat.Problem.BSP.Common
import Data.Word
import HSat.Problem.BSP.Common.Clause.Internal
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Set (Set)

name :: String
name = "Clause"

tests :: TestTree
tests =
  testGroup name [
    testGroup "makeClause" [
       makeClauseTest1,
       makeClauseTest2,
       makeClauseTest3
       ]
    ]

makeClauseTest1 :: TestTree
makeClauseTest1 =
  testProperty "makeClause >=1 True" $ ioProperty $ do
    size <- generate $ choose (0,100)
    s <- generate arbitrary
    set <- mkLiteralSet 100 s
    result <- f (makeClause Any size) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right ((b,clause),_) ->
        let n = checkClause clause (getTrueSet set)
            trivial = property $ if (not s) then not $ triviallyTrue clause else True
        in (property $ (
          if b then
            (n == size) else
            if size == 0 then
              True else
              n >= 1)) .&&. trivial

f :: (MonadRandom m) => LiteralMake m a -> LiteralSet -> m (Either LiteralMakeError (a,LiteralSet))
f func init = runEitherT (runStateT func init)

checkClause :: Clause -> (Map Variable Sign) -> Word
checkClause (Clause vect _) m =
  V.foldl f 0 vect
  where
    f :: Word -> Literal -> Word
    f n l =
      if (m M.! (getVariable l) == (getSign l)) then (n+1) else n
      
makeClauseTest2 :: TestTree
makeClauseTest2 =
  testProperty "checkClause All" $ ioProperty $ do
    size <- generate $ choose (0,100)
    s <- generate arbitrary
    set <- mkLiteralSet 100 s
    result <- f (makeClause All size) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right ((b,clause),_) ->
        let n = checkClause clause (getTrueSet set)
            trivial = property $ if (not s) then not $ triviallyTrue clause else True
        in property $ (
          if b then
            (n==size) else
            False) .&&. trivial

makeClauseTest3 :: TestTree
makeClauseTest3 =
  testProperty "checkClause None" $ ioProperty $ do
    size <- generate $ choose (0,100)
    s <- generate arbitrary
    set <- mkLiteralSet 100 s
    result <- f (makeClause None size) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right ((b,clause),_) ->
        let n = checkClause clause (getTrueSet set)
            trivial = property $ if (not s) then not $ triviallyTrue clause else True
        in property $ (
          if b then
            (n==size) else
            True) .&&. trivial

triviallyTrue :: Clause -> Bool
triviallyTrue (Clause vect _) = triviallyTrue' vect S.empty
  where
    triviallyTrue' :: V.Vector Literal -> Set Variable -> Bool
    triviallyTrue' vect s =
      if vect == V.empty then False else
        let var = getVariable . V.head $ vect
            vect' = V.tail vect
        in if S.member var s then
             True else
             triviallyTrue' vect' (S.insert var s)
