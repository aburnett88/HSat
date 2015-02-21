module HSat.Make.BSP.Common.Literal (
  makeLiteral,
  LiteralMakeError(..),
  LiteralMakeStatus(..),
  LitGenSet(..),
  LiteralStatusError(..),
  mkVarSet,
  mkLitSet,
  mkNoSet,
  getSize
  ) where

import Data.Word
import Control.Monad.Trans.Either
import Control.Monad.State
import Control.Monad.Random
import HSat.Problem.BSP.Common.Literal
import qualified Data.Set as S
import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Common.Sign

data LiteralMakeStatus = LiteralMakeStatus {
  getTotalLitsLeftToMk :: Word,
  getGenerateSet       :: LitGenSet
  } deriving (Eq,Show)

getSize :: LitGenSet -> Word
getSize (VariableSet s _) = toEnum $ S.size s
getSize (LiteralSet   s _) = toEnum $ S.size s
getSize _ = 0

mkVarSet,mkLitSet,mkNoSet :: Word -> LitGenSet
mkVarSet n =
  VariableSet set n
  where
    set = S.fromList $ map mkVariable [1..n]
mkLitSet n =
  LiteralSet set n
  where
    set = S.fromList $ posLits ++ negLits
    posLits = map (mkLiteral pos) . map mkVariable $ [1..n]
    negLits = map (mkLiteral neg) . map mkVariable $ [1..n]
mkNoSet n = NoSet n

data LitGenSet =
  VariableSet (S.Set Variable) Word |
  LiteralSet  (S.Set Literal ) Word |
  NoSet                        Word
  deriving (Eq,Show)

genAny :: (MonadRandom m) => LitGenSet -> m Literal
genAny litGen = do
  let max = case litGen of
        NoSet w -> w
        LiteralSet _ w -> w
        VariableSet _ w -> w
  var <- mkVariable `liftM` getRandomR (1,max)
  sign <- mkSign `liftM` getRandom
  return $ mkLiteral sign var

type LiteralStatusError random result =
  StateT LiteralMakeStatus (EitherT LiteralMakeError random) result

makeLiteral :: (MonadRandom m) => LiteralStatusError m Literal
makeLiteral = do
  litConfig <- get
  let totalLitsLeftToMk = getTotalLitsLeftToMk     litConfig
      genSetSize        = getSize $ getGenerateSet litConfig
  case compare genSetSize totalLitsLeftToMk of
    GT -> lift . left $ CannotFulfillCallsRemaining genSetSize totalLitsLeftToMk
    EQ -> takeFromSet
    LT -> takeFromAny

takeFromSet :: (MonadRandom m) => LiteralStatusError m Literal
takeFromSet = do
  return $ mkLiteral pos (mkVariable 1)

takeFromAny :: (MonadRandom m) => LiteralStatusError m Literal
takeFromAny = do
  set <- gets getGenerateSet
  result <- genAny set
  modify (flip update result)
  return result

update :: LiteralMakeStatus -> Literal -> LiteralMakeStatus
update (LiteralMakeStatus w lgen) l = (LiteralMakeStatus (w-1) (removeFromSet lgen l))
  
removeFromSet :: LitGenSet -> Literal -> LitGenSet
removeFromSet (VariableSet s w) l =
  let s' = S.delete (getVariable l) s
  in VariableSet s' w
removeFromSet (LiteralSet s w) l =
  let s' = S.delete l s
  in LiteralSet s' w
removeFromSet n _ = n
  
  
data LiteralMakeError =
  CannotFulfillCallsRemaining Word Word
  deriving (Eq,Show)
