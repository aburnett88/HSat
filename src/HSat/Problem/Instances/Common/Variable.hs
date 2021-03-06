{-|
Module      : HSat.Problem.Instances.Common.Variable
Description : The Variable data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module describes and exports the common functions associated with the
'Variable' data type. Typically its use is to be used as a numerical
description of a free variable within a problem.
-}
module HSat.Problem.Instances.Common.Variable (
  -- * Data Type
  Variable,
  getWord,               -- :: Variable -> Word
  -- * Constructors for Variable
  mkVariable,            -- :: Word -> Variable
  mkVariableFromInteger, -- :: Integer -> Variable
  -- * Utility Functions
  varInRange,            -- :: Word -> Variable -> Bool
  variableToInteger      -- :: Variable -> Integer
  ) where

import HSat.Problem.Instances.Common.Variable.Internal

name :: String
name = "HSat.Problem.BSP.Common.Literal"
             
{-|
Constructs a 'Variable' from a 'Word'.

Throws an error if the argument is zero
-}
mkVariable      :: Word -> Variable
mkVariable 0    = error (name ++ ":mkVariable. Argument zero")
mkVariable word = Variable word

{-|
Constructs a 'Variable from an 'Integer'.

Throws a run time error if the value is 0, or above the maximum
value for a 'Word'
-}
mkVariableFromInteger         :: Integer -> Variable
mkVariableFromInteger integer =
  let name' = ":mkVariableFromInteger. Argument "
      absInteger = case compare integer 0 of
        EQ -> error (name ++ name' ++ show integer)
        LT -> abs integer
        GT -> integer
  in case compare absInteger maxWord of
    GT -> error (name ++ name' ++ show integer)
    _  -> Variable $ fromInteger absInteger

{-|
The maximum 'Integer' representation of a 'Word'. It is used to deduce whether
an error should be thrown.

It is put through the constructor for mkVariable to make sure that no run-time
errors are thrown
-}
maxWord :: Integer
maxWord = toInteger . getWord . mkVariable $ maxBound

{-|
Takes an initial argument x. Returns 'True' if the underlying 'Word' within
the 'Variable' is less than or equal to x
-}
varInRange                       :: Word -> Variable -> Bool
varInRange 0     _               = False
varInRange range (Variable word) = word <= range

{-|
Constructs an 'Integer' from a 'Variable'
-}
variableToInteger :: Variable -> Integer
variableToInteger = toInteger . getWord
