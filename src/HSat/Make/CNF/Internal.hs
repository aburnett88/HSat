{-|
Module      : HSat.Make.CNF.Internal
Description : The internal functions that help to make CNF Problems
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides each of the main combinators to create 'CNF' problems
-}

module HSat.Make.CNF.Internal (
  -- * Initialisation of Data
  ClausesInit(..), 
  -- * Populating data randomly
  chooseClauses, -- :: (MonadRandom m) => ClausesInit -> EitherT CNFMakeError m Clauses
  chooseNumbVariables,
  chooseNumbClauses,
  chooseEachClauseSize,
  -- * Errors for when things go wrong
  CNFMakeError(..)
  ) where

import           Control.Monad (replicateM,liftM,unless)
import           Control.Monad.Random
import           Control.Monad.Random.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Set as S
import           Data.Word
import           HSat.Make.Config as Config
import           HSat.Make.Internal
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.CNF.Builder
import           HSat.Problem.BSP.Common

{-|
This is a simple wrapper type for the input to 'chooseClauses'.

Invariants:
  sum 'getLitClSize' == getVarTotal
-}
data ClausesInit = ClausesInit {
  -- | List containing the desired size of each 'Clause' to be created
  getListClSize :: [Word]           ,
  -- | Total number of 'Variable's to be randomly generated
  getVarTotal   :: Word             ,
  -- | Maximum 'Word' representation that is legal when creating 'Variable's
  getVarNumb    :: Word             ,
  -- | Predicate that should hold regarding occurences of 'Variable's
  getVarPred    :: VariablePredicate
  } deriving (Eq,Show)

{-|
Describes Error's that can be thrown when creating 'CNF' 'Problem's. 
-}
data CNFMakeError =
  -- | Is thrown when the random creator wants to create more 'Literal's, but the number of remaining calls computed initially has reached zero
  CallsRemainingFinished    |
  -- | Is thrown when the random creator is unable to choose a number of 'Variable's consistnat with the 'CNFConfig'
  UnableToChooseVariables   |
  -- | Thrown when the random creator is unable to choose a set of sizes for 'Clause's that is consistant with the 'CNFConfig'
  UnableToChooseClauseSizes 
  deriving (Eq,Show)

{-|
Populates a set of 'Clauses' with data within 'ClausesInit'. 
-}
chooseClauses             :: (MonadRandom m) => ClausesInit -> EitherT CNFMakeError m Clauses
chooseClauses clausesInit = do
  let listClSize = getListClSize clausesInit
      varTotal   = getVarTotal clausesInit
      varNumb    = getVarNumb clausesInit
      varPred    = HSat.Make.CNF.Internal.getVarPred clausesInit
      initStatus = mkStatus varNumb varTotal varPred
  evalStateT (foldM runChooseClauses mkClauses listClSize) initStatus


{-|
This data type describes the current Status of the Random creation.
-}
data CNFMakeStatus = CNFMakeStatus {
  -- | The Set of Vars or Lit's which need to be used, but currently have not been used
  getNotUsed   :: ToBeUsedSet,
  -- | The number of calls remaining where a random Literal will be created
  getCallsLeft :: Word       ,
  -- | The maximum 'Variable' allowed to be created
  getMaxVar    :: Word
  } deriving (Show)

--Modifiers

{-|
Remove's the 'Literal' from the set, and reduces the 'getCallsLeft' by 1
-}
rmLiteral                    :: Literal -> CNFMakeStatus -> CNFMakeStatus
rmLiteral literal makeStatus =
  makeStatus {
    getNotUsed   = rmLiteral' (getNotUsed makeStatus) literal,
    getCallsLeft = getCallsLeft makeStatus - 1
  }
                   
{-|
The ToBeUsed Set is a Set of what must be added to the Problem before generation is finished.
-}
data ToBeUsedSet =
  -- | The 'Set' of 'Variable's that must be included
  VariableSet (S.Set Variable) |
  -- | The 'Set' of 'Literal's that must be included
  LiteralSet  (S.Set Literal)  |
  -- | When nothing must be included
  NoSet
  deriving (Eq,Show)

rmLiteral'                              :: ToBeUsedSet -> Literal -> ToBeUsedSet
rmLiteral' NoSet                _       = NoSet
rmLiteral' (VariableSet varSet) literal =
  VariableSet $ S.delete (getVariable literal) varSet
rmLiteral' (LiteralSet  litSet) literal =
  LiteralSet $ S.delete literal litSet

{-|
Returns the size of the remaining elements that have not been inserted int he problem, but most be
-}
getSize               :: CNFMakeStatus -> Word
getSize cnfMakeStatus =
  case getNotUsed cnfMakeStatus of
    VariableSet s -> toEnum $ S.size s
    LiteralSet  s -> toEnum $ S.size s
    NoSet         -> 0

{-|
Constructor to initialise the CNFMakeStatus. Most computatin is creating the ToBeUsed Set
-}
mkStatus                          :: Word -> Word -> VariablePredicate -> CNFMakeStatus
mkStatus varNumb varTotal varPred =
  let notUsed = mkToBeUsedSet varTotal varNumb varPred
  in CNFMakeStatus notUsed varTotal varNumb

{-|
Populate ToBeUsed based upon the VariablePredicate. If it is 'AtLeastOnce', we give a list of 'Variable's that must be added. If its 'PosAndNeg' we populate it with 'Literal's of the positive and negative of each 'Variable'. If no predicate, then NoSet s used.
-}
mkToBeUsedSet                                     :: Word -> Word -> VariablePredicate -> ToBeUsedSet
mkToBeUsedSet 0        _              _           = NoSet
mkToBeUsedSet _        _              NoPredicate = NoSet
mkToBeUsedSet varTotal varNumb        varPred     =
  let varSet = S.fromList . map mkVariable $ [1 .. varNumb]
  in case varPred of
    AtleastOnce -> VariableSet varSet
    PosAndNeg   ->
      let posLitSet = S.map (mkLiteral pos) varSet
          negLitSet = S.map (mkLiteral neg) varSet
      in LiteralSet $ posLitSet `S.union` negLitSet

{-|
This simplified data type hides the complexity behind many of the below functions
-}
type CNFStatusErr random result =
  StateT CNFMakeStatus (EitherT CNFMakeError random) result
  
{-|
Takes a set of 'Clauses', a 'Word' denoting size, and adds a 'Clause' to the 'Clauses' that has the size of the 'Word'
-}
runChooseClauses                :: (MonadRandom m) => Clauses -> Word -> CNFStatusErr m Clauses
runChooseClauses clauses clSize = clausesAddClause clauses `liftM` chooseClause clSize emptyClause

{-|
Adds a randomly generated 'Literal' to the 'Clause' until the counter reaches zero
-}
chooseClause          :: (MonadRandom m) => Word -> Clause -> CNFStatusErr m Clause
chooseClause 0 clause = return clause
chooseClause n clause = chooseLiteral >>= chooseClause (n-1) . clauseAddLiteral clause

{-|
Allows us to choose a 'Literal'.

If there are no more calls left
  throw an error
If the number of calls left is less than the number left in the notused set
  throw an error
If the number of calls left is equal to the number left in the unsued set
  then take randomly from the unused set
otherwise, take randomly, but remove that literal from the tobeused set
-}
chooseLiteral :: (MonadRandom m) =>  CNFStatusErr m Literal
chooseLiteral = do
  status <- get
  let callsLeft   = getCallsLeft status
      sizeUnused  = getSize status
      returnError = lift $ left CallsRemainingFinished
  case compare sizeUnused callsLeft of
    GT -> returnError
    EQ -> takeFromSet
    LT -> takeFromAny

{-|
Take from the set of NotUsed. Get the random Literal, then remove it through modification, then return the 'Literal'
-}
takeFromSet :: (MonadRandom m) => CNFStatusErr m Literal
takeFromSet = do
  notUsed <- gets getNotUsed
  literal <- getRandomLit notUsed
  modify (rmLiteral literal)
  return literal

{-|
Get a random Literal, make a random sign, create the literal, remove it from the set
-}
takeFromAny :: (MonadRandom m) => CNFStatusErr m Literal
takeFromAny = do
  maxVar   <- gets getMaxVar
  variable <- getRandomR (1, maxVar)
  sign     <- getRandom
  let literal = mkLiteral sign $ mkVariable variable
  modify (rmLiteral literal)
  return literal

{-|
Get the index of the element from the set

If it is a 'Variable'
  Create random Sign S
  Create Literal L
  Remove from Set
If it is a 'Literal'
  Remove from Set
-}
getRandomLit                      :: (MonadRandom m) => ToBeUsedSet -> m Literal
getRandomLit (VariableSet varSet) = do
  index <- getRandomR (0, S.size varSet)
  sign  <- getRandom
  let variable = S.elemAt index varSet
  return $ mkLiteral sign variable
getRandomLit (LiteralSet litSet)  = do
  index <- getRandomR (0, S.size litSet)
  let literal = S.elemAt index litSet
  return literal
getRandomLit NoSet                = error "getRandomLit: Argument NoSet"

{-|
Chooses a random 'Word' from the bounds denoted in 'CNFConfig'
-}
chooseNumbClauses :: (MonadRandom m) => StateT CNFConfig m Word
chooseNumbClauses = do
  clauseNumber <- gets getClNum
  evalBounded clauseNumber

{-|
Checks to see if the bounds can be adhered to. If not, then new bounds are formulated as close as possible to the original, then the 'Word' returned that adheres to this
-}
chooseNumbVariables             :: (MonadRandom m) => Word -> Word -> StateT CNFConfig m Word
chooseNumbVariables total clNum = do
  vpc <- gets getVarNum
  let boundWords = boundedToWords clNum vpc
  changeVarNumbIfNeeded total boundWords
  vpc' <- gets getVarNum
  let boundWords' = boundedToWords clNum vpc'
  evalBounded boundWords'

{-|
Takes a 'Word' denoting the number of 'Clause', and a 'VariableNumber', and returns a 'Word' that adheres to the specification
-}
boundedToWords                   :: Word -> VariableNumber -> Bounds Word
boundedToWords _ (Right w)       = w
boundedToWords cl (Left doubles) =
  let l = getLesser doubles
      r = getGreater doubles
  in mkBounds (posDoubleToWord cl l) (posDoubleToWord cl r)

{-|
Converts a 'Double' to a 'Word'. Makes sure that rounding errors do not occour. 
-}
doubleToWord   :: Double -> Word
doubleToWord d =
  let integerVal = round d
      maxWord    = fromIntegral (maxBound :: Word)
  in case compare maxWord integerVal of
    LT -> maxBound
    _  -> fromIntegral integerVal

{-|
Converts a 'PosDouble' to a 'Word' that is relative to the initial 'Word' argument following the follwoing equation.

INPUT: w, d
result = w/p
-}
posDoubleToWord     :: Word -> PosDouble -> Word
posDoubleToWord w p = doubleToWord . (w' /) . getDouble $ p
  where
    w' = wordToDouble w

{-|
Converts a 'Word' to its closest representation of 'Double'
-}
wordToDouble :: Word -> Double
wordToDouble = fromIntegral

{-|
When given a new 'Bounds Word' representing a new 'getVarNum' value, replaces this withint he 'CNFConfig'
-}
modifyNumbVariables           :: Bounds Word -> CNFConfig -> CNFConfig
modifyNumbVariables vn config =
  config {
    getVarNum = Right vn
    }

{-|
A simple placeholder for a very simple 'Bounds Word' with minimum and maximum value '0'
-}
zero :: Bounds Word
zero = mkBounds 0 0

{-|
Updates the 'getVarNum' of the 'CNFConfig' within the 'State' if it is inconstant with the total number of 'Variable's that need to be generated
-}
changeVarNumbIfNeeded              :: (Monad m) => Word -> Bounds Word -> StateT CNFConfig m ()
changeVarNumbIfNeeded 0 worded     =
  unless (worded == zero) $ modify (modifyNumbVariables zero)
changeVarNumbIfNeeded total worded = do
  predicate <- gets Config.getVarPred
  let minAllowed = 1
      maxAllowed = case predicate of
        NoPredicate -> maxBound
        AtleastOnce -> total
        PosAndNeg   -> total `div` 2
      currMin    = getLesser worded
      currMax    = getGreater worded
      newMin
        | currMin < minAllowed = minAllowed
        | currMin > maxAllowed = maxAllowed
        | otherwise            = currMin
      newMax
        | currMax > maxAllowed = maxAllowed
        | currMax < minAllowed = minAllowed
        | otherwise            = currMax
  unless (currMin == newMin && currMax == newMax) $ modify $ modifyNumbVariables $ mkBounds newMin newMax

{-|
Generates a set of 'Word' that is consistnat with the input values and the 'getClSize' of the 'CNFConfig'
-}
chooseEachClauseSize          :: (MonadRandom m) => Word -> StateT CNFConfig m ([Word],Word)
chooseEachClauseSize clNumber = do
  clSize <- gets getClSize
  --Corner cases suck.
  --If we are making a CNF with true an false lits, and totalVars == 1 then totalVars must equal 2
  tuple@(clSizes,total) <- evalClauses' clNumber ([],0)
  predicate <- gets Config.getVarPred
  case (predicate,total,clNumber) of
    (PosAndNeg,1,1) -> do
      modify modifyVPC
      return ([2],2)
    (PosAndNeg,1,_) ->
      return (replace' clSizes,2)
    _ -> return tuple

{-|
Given a 'Word' n, creates a random list of 'Word' that are within the 'getClSize' bounds. Also returns the total within the tuple as the second argument
-}
evalClauses'              :: (MonadRandom m) => Word -> ([Word],Word) -> StateT CNFConfig m ([Word],Word)
evalClauses' 0 result     = return result
evalClauses' n (xs,total) = do
  bounds <- gets getClSize
  x <- evalBounded bounds
  evalClauses' (n-1) (x:xs,x+total)
      
{-|
A very specific function; replaces the first 'Word' found that equals '0' with a '1', and returns the list. Throws an error on an empty list
-}
replace'        :: [Word] -> [Word]
replace' []     = error "replace: Empty list"
replace' (0:xs) = 1:xs
replace' (x:xs) = x : replace' xs

{-|
Takes a 'CNFConfig' and replaces the higher bound of its 'getClSize' with a '2' if it is below '2'
-}
modifyVPC        :: CNFConfig -> CNFConfig
modifyVPC config =
  let clSize  = getClSize config
      clSize' = case compare 2 (getGreater clSize) of
        GT -> mkBounds (getLesser clSize) 2
        _  -> clSize
  in config {
    getClSize = clSize'
    }
--346
