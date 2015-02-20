module HSat.Make.BSP.Common.Literal (
  makeLiteral,
  LiteralMakeError(..),
  LiteralMakeStatus(..)
  ) where

import Data.Word
import Control.Monad.Trans.Either
import Control.Monad.State
import Control.Monad.Random
import HSat.Problem.BSP.Common.Literal

data LiteralMakeStatus = LiteralMakeStatus {
  getTotalLitsLeftToMk :: Word,
  getGenerateSet       :: LitGenSet
  } deriving (Eq,Show)

getSize :: LitGenSet -> Word
getSize _ = 0

data LitGenSet = LitGenSet
  deriving (Eq,Show)

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

takeFromSet = undefined

takeFromAny = undefined
  
data LiteralMakeError =
  CannotFulfillCallsRemaining Word Word
  deriving (Eq,Show)
