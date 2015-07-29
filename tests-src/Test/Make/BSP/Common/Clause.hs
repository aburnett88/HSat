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
import HSat.Problem.BSP.Common.Clause.Internal
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Set (Set)



sss :: IO (Bool,LiteralSet,Word)
sss = do
  size <- generate $ choose (0,100)
  s <- generate arbitrary
  set <- mkLiteralSet 100 s
  return (s,set,size)

makeClauseTest1 :: TestTree
makeClauseTest1 =
  testProperty "makeClause >=1 True" $ ioProperty $ do
    (s,set,size) <- sss
    result <- run (makeClause Any size) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right ((b,clause),_) ->
        let n = checkClause clause (getTrueSet set)
            trivial = property $ s || not (triviallyTrue clause)
        in (property (
          if b then
            n == size else
            size == 0 || (n >= 1)) .&&. trivial)

run :: (MonadRandom m) => LiteralMake m a -> LiteralSet ->
       m (Either LiteralMakeError (a,LiteralSet))
run func initial = runEitherT (runStateT func initial)

checkClause :: Clause -> Map Variable Sign -> Word
checkClause (Clause vect _) m =
  V.foldl f 0 vect
  where
    f :: Word -> Literal -> Word
    f n l =
      if m M.! getVariable l == getSign l then n+1
      else n
      
makeClauseTest2 :: TestTree
makeClauseTest2 =
  testProperty "checkClause All" $ ioProperty $ do
    (s,set,size) <- sss
    result <- run (makeClause All size) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right ((b,clause),_) ->
        let n = checkClause clause (getTrueSet set)
            trivial = property (s || not (triviallyTrue clause)) 
        in property $ (b && n==size) .&&. trivial
           
makeClauseTest3 :: TestTree
makeClauseTest3 =
  testProperty "checkClause None" $ ioProperty $ do
    (s,set,size) <- sss
    result <- run (makeClause None size) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right ((b,clause),_) ->
        let n = checkClause clause (getTrueSet set)
            trivial = property $ s || not (triviallyTrue clause) 
        in property $ (
          not b || (n == size)) .&&. trivial

triviallyTrue :: Clause -> Bool
triviallyTrue (Clause vectLits _) = triviallyTrue' vectLits S.empty
  where
    triviallyTrue' :: V.Vector Literal -> Set Variable -> Bool
    triviallyTrue' vect s =
      (vect /= V.empty) &&
      (let var = getVariable . V.head $ vect
           vect' = V.tail vect
       in S.member var s ||
          triviallyTrue' vect' (S.insert var s)
          )
