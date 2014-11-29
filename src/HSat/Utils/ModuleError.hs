{-|
Module      : HSat.Utils.ModuleError
Description : A method of generating pretty error messages
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Thos module provides utility functions for generating
pretty error messages. 

-}

module HSat.Utils.ModuleError (
  moduleErr
  ) where

{-|
This function makes throwing errors slightly easier;
it takes a location, a function name and a message and
fills in the punctuation correctly for the user
-}
moduleErr                       :: String ->
                                   String ->
                                   String ->
                                   String
moduleErr location function msg =
  location ++ ('.':function) ++ (':':' ':msg)
