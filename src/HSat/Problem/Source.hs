{-|
Module      : HSat.Problem.Source
Description : A type to represent the 'Source' of 'Problem's
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains common functions that act on 'Source' data types
-}

module HSat.Problem.Source (
  Source,
  -- * Constructors
  mkStatic    , -- :: Source
  mkFileSource, -- :: FilePath -> Source
  mkMakeConfig, -- :: Config -> Source
  ) where

import HSat.Make.Config.Class
import HSat.Problem.Source.Internal

{-|
Constructs a static 'Source'
-}
mkStatic :: Source
mkStatic = StaticSource

{-|
Constructs a 'Source' from a 'FilePath'
-}
mkFileSource :: FilePath -> Source
mkFileSource = FileSource

{-|
Constructs a 'Source' from a 'Config'
-}
mkMakeConfig :: Config -> Source
mkMakeConfig = MakeConfiguration
