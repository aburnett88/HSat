{-|
Module      : TestUtils.Validate
Description : The Validate type class
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

'Validate' provides a definition for validating a data structure. For example,
we may cache information about the size of a list as it is created. Validate
would check that this is correct
-}

module TestUtils.Validate (
  Validate(..)
  ) where

{-|
The 'Validate' class provides a validate method for types, to make
sure that they have been defined correctly.
-}
class Validate a where
  -- | validate should return a 'Bool' which defines whether a data structure
  -- | is valid
  validate :: a -> Bool
