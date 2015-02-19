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
  mkFileSource  -- :: FilePath -> Source
  ) where

import HSat.Printer

{-|
A data type describing the source of a problem. 
-}
data Source =
  -- | The source can be a static source - written in code perhaps, or as a
  -- |placeholder
  StaticSource |
  -- | When the source has come from a file
  FileSource FilePath
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

instance Printer Source where
  compact StaticSource = text "STATIC"
  compact (FileSource fp) =
    text "FILE:" <+>
    text (
      case compare 20 (length fp) of
        LT -> fp
        _ -> take 20 fp ++ "..."
        )
  noUnicode StaticSource = text "Static Source"
  noUnicode (FileSource fp) =
    text "FilePath:" <+>
    text fp
  unicode StaticSource = green (text "StaticSource")
  unicode (FileSource fp) =
    text "FilePath:" <+>
    (yellow . text $ fp)
