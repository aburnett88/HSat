{-|
Module      : HSat.Problem.Source
Description : A type to represent the 'Source' of 'Problem's
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This odule lays down the functionality for representing 'Source's of 'Problem's
-}

module HSat.Problem.Source (
  Source(..),
  -- * Constructors
  mkStatic,     -- :: Source
  mkFileSource, -- :: FilePath -> Source
  mkMakeConfig  -- :: Config -> Source
  ) where

import HSat.Printer
import HSat.Make.Config

{-|
A data type describing the source of a problem. 
-}
data Source =
  -- | The source can be a static source - written in code perhaps, or as a
  -- |placeholder
  StaticSource |
  -- | When the source has come from a file
  FileSource FilePath |
  MakeConfiguration Config
  deriving (Eq,Show)

{-|
A quick constructor for a static file source
-}
mkStatic :: Source
mkStatic = StaticSource

{-|
A quick constructor for a source from a file
-}
mkFileSource :: FilePath -> Source
mkFileSource = FileSource

{-|
Takes a 'Config' and wraps it in a 'Source' datatype
-}
mkMakeConfig :: Config -> Source
mkMakeConfig = MakeConfiguration

instance Printer Source where
  compact StaticSource = text "STATIC"
  compact (FileSource fp) =
    text "FILE:" <+>
    text (
      case compare 20 (length fp) of
        LT -> fp
        _ -> take 20 fp ++ "..."
        )
  compact (MakeConfiguration m) =
    text "MAKE:" <+>
    (text . show $ m)
  noUnicode StaticSource = text "Static Source"
  noUnicode (FileSource fp) =
    text "FilePath:" <+>
    text fp
  noUnicode (MakeConfiguration m) =
    text "Make Config:" <+>
    (text . show $ m)
  unicode StaticSource = green (text "StaticSource")
  unicode (FileSource fp) =
    text "FilePath:" <+>
    (yellow . text $ fp)
  unicode (MakeConfiguration m) =
    text "Make Configuration:" <+>
    (text . show $ m)
