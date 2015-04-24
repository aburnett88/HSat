module HSat.Make.BSP.Common.Literal (
  LiteralSet(..),
  mkLiteralSet,
  reset,
  getTrueLiteral,
  getRandomLiteral,
  LiteralMake,
  LiteralMakeError(..),
  LiteralPredicate(..)
  ) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Random
import HSat.Problem.BSP.Common
import Control.Monad.Trans.Either
import Control.Monad.State

data LiteralSet = LiteralSet {
  getVarsThatCanAppear :: Set Variable,
  getTrueSet           :: Map Variable Sign,
  getHasGeneratedTrue  :: Word,
  getMaximumVariable   :: Word,
  getVarsAppearTwice   :: Bool
  } deriving (Eq,Show)

mkLiteralSet :: (MonadRandom m, Applicative m) => Word -> Bool -> m LiteralSet
mkLiteralSet 0 vAppearTwice = return $
                 LiteralSet S.empty M.empty 0 0 vAppearTwice
mkLiteralSet maxVar vAppearTwice =
  (\ts -> LiteralSet vars ts 0 maxVar vAppearTwice) <$> mkTrueSet
  where
    vars = S.fromList varList
    varList = map mkVariable [1..maxVar]
    mkTrueSet :: (MonadRandom m, Applicative m) => m (Map Variable Sign)
    mkTrueSet = M.fromList <$>
                mapM (\var -> ((,) var . mkSign) <$> getRandom) varList

data LiteralMakeError =
  CannotFindMapping |
  NoVariables
  deriving (Eq,Show)

type LiteralMake random result =
  StateT LiteralSet (EitherT LiteralMakeError random) result

reset :: (MonadRandom m) => LiteralMake m ()
reset =
  modify reset'
  where
    reset' :: LiteralSet -> LiteralSet
    reset' ls =
      ls {
        getVarsThatCanAppear = fullSet ls,
        getHasGeneratedTrue  = 0
        }

fullSet :: LiteralSet -> S.Set Variable
fullSet ls = vars
  where
    maxVariable = getMaximumVariable ls
    vars = case maxVariable of
      0 -> S.empty
      _ -> S.fromList $ map mkVariable [1..maxVariable]

makeVariable :: (MonadRandom m) => LiteralMake m Variable
makeVariable = do
  vars <- gets getVarsThatCanAppear
  case S.size vars of
    0 -> lift $ left NoVariables
    n -> do
      vAppearTwice <- gets getVarsAppearTwice
      index <- getRandomR (0,n-1)
      let var = S.elemAt index vars
      unless vAppearTwice $ modify $ removeVariable var
      return var

removeVariable :: Variable -> LiteralSet -> LiteralSet
removeVariable v ls =
  let varsThatCanAppear = getVarsThatCanAppear ls
  in ls {
    getVarsThatCanAppear = S.delete v varsThatCanAppear
    }

getTrueLiteral :: (MonadRandom m) => LiteralMake m Literal
getTrueLiteral = do
  var <- makeVariable
  mapping <- gets getTrueSet
  case M.lookup var mapping of
    Nothing -> lift $ left CannotFindMapping
    Just sign -> do
      modify changeTrueLiteralCreated
      return $ mkLiteral sign var

changeTrueLiteralCreated :: LiteralSet -> LiteralSet
changeTrueLiteralCreated ls =
  ls {
    getHasGeneratedTrue = getHasGeneratedTrue ls + 1
  }

getRandomLiteral :: (MonadRandom m) => LiteralMake m Literal
getRandomLiteral = do
  var <- makeVariable
  bool <- getRandom
  let sign = mkSign bool
      lit  = mkLiteral sign var
  mapping <- gets getTrueSet
  case M.lookup var mapping of
    Just sign' -> if sign == sign' then do
                    modify changeTrueLiteralCreated
                    return lit else
                    return lit
    Nothing -> lift $ left CannotFindMapping
  
data LiteralPredicate =
  Any |
  All |
  None
  deriving (Eq,Show)
