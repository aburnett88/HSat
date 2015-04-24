module Test.Make.BSP.Common.Clauses (
  tests
  ) where

import TestUtils
import Control.Monad.Random
import Control.Monad.Trans.Either
import Control.Monad.State
import HSat.Make.BSP.Common.Literal
import HSat.Make.BSP.Common.Clauses
import qualified Data.Vector as V
import HSat.Problem.BSP.Common
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

name :: String
name = "Clauses"

tests :: TestTree
tests =
  testGroup name [
    testGroup "makeClauses" [
       makeClausesTest1,
       makeClausesTest2,
       makeClausesTest3
       ]
    ]

run :: (MonadRandom m) => LiteralMake m a -> LiteralSet ->
       m (Either LiteralMakeError (a,LiteralSet))
run func initial = runEitherT (runStateT func initial)

makeClausesTest1 :: TestTree
makeClausesTest1 =
  testProperty "testClauses No predicate" $ ioProperty $ do
    (s,set,sizes) <- sss
    result <- run (makeClauses sizes None) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right (clauses,_) ->
        let trivials = property $ s || V.all
                       (not . triviallyTrue) (getVectClause clauses)
        in trivials

sss :: IO (Bool,LiteralSet,[Word])
sss = do
  s <- generate arbitrary
  set <- mkLiteralSet 100 s
  sizes <- generate $ listOf $ choose (0,100)
  return (s,set,sizes)

makeClausesTest2 :: TestTree
makeClausesTest2 =
  testProperty "testClauses All have atleast one true" $ ioProperty $ do
    (s,set,sizes) <- sss
    result <- run (makeClauses sizes All) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected left: " ++ show e) False
      Right (clauses,_) ->
        let m = getTrueSet set
            trivials = counterexample
                       (show (V.all (
                                 not . triviallyTrue) (getVectClause clauses
                                                          )
                             )
                       )
                       (
              s ||
              V.all (not . triviallyTrue ) (getVectClause clauses)
              )
            allHaveOneTrue = counterexample
                             (show $ V.zip
                              (getVectClause clauses)
                              (V.map (`oneTrue` m) (getVectClause clauses))) $
                             V.all (`oneTrue` m) (getVectClause clauses)
        in trivials .&&. allHaveOneTrue

makeClausesTest3 :: TestTree
makeClausesTest3 =
  testProperty "makeClause Any - Atleast one has all set to true" $
  ioProperty $ do
    sizes <- generate $ listOf $ choose (0,100)
    s <- generate arbitrary
    set <- mkLiteralSet 100 s
    result <- run (makeClauses sizes Any) set
    return $ case result of
      Left e ->
        counterexample ("Unexpected Left: " ++ show e) False
      Right (clauses,_) ->
        let m = getTrueSet set
            trivials = property (
                s || V.all (not . triviallyTrue) (getVectClause clauses)
              )
            allClauses = (V.null (getVectClause clauses) ||
                           V.any (`allTrue` m) (getVectClause clauses))
        in trivials .&&. allClauses

allTrue :: Clause -> Map Variable Sign -> Bool
allTrue c m =
  let vect = getVectLiteral c
  in (V.null vect || V.all f vect)
  where
    f :: Literal -> Bool
    f l = (m M.! getVariable l) == getSign l

oneTrue :: Clause -> Map Variable Sign -> Bool
oneTrue c m =
  let vect = getVectLiteral c
  in (V.null vect || V.any f vect)
  where
    f :: Literal -> Bool
    f l = (m M.! getVariable l) == getSign l

triviallyTrue :: Clause -> Bool
triviallyTrue cl = triviallyTrue' (getVectLiteral cl) S.empty
  where
    triviallyTrue' :: V.Vector Literal -> Set Variable -> Bool
    triviallyTrue' vect s =
      (vect /= V.empty) &&
      (let var = getVariable . V.head $ vect
           vect' = V.tail vect
       in (S.member var s || triviallyTrue' vect' (S.insert var s))
          )
