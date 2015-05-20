{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HSat.Problem.Source.Internal
Description : Internal representation of the Source type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports the internal representation of the Source data type
-}

module HSat.Problem.Source.Internal (
  Source(..)
  ) where

import HSat.Make.Config
import HSat.Printer

{-|
A data type describing the source of a problem. 
-}
data Source =
  -- | The source can be a static source - written in code perhaps, or as a
  -- |placeholder
  StaticSource             |
  -- | When the source has come from a file
  FileSource FilePath      |
  -- | Constructor for a problem made from a configuration
  MakeConfiguration Config
  deriving (Eq)

instance Show Source where
  showsPrec = show'


lenSize :: Int
lenSize = 20

instance Printer Source where
  compact StaticSource = "STATIC"
  compact (FileSource fp) =
    "FILE:" <+>
    (
      case compare lenSize (length fp) of
        LT -> text fp
        _  -> text $ take lenSize fp ++ "..."
    )
  compact (MakeConfiguration m) =
    "MAKE:" <+>
    (pTypeToDoc Compact m)
  noUnicode StaticSource = "Static Source"
  noUnicode (FileSource fp) =
    "FilePath:" <+>
    text fp
  noUnicode (MakeConfiguration m) =
    "Make Config:" <+>
    (pTypeToDoc NoUnicode m)
  unicode StaticSource = green "Static Source"
  unicode (FileSource fp) =
    "FilePath:" <+>
    (yellow . text $ fp)
  unicode (MakeConfiguration m) =
    "Make Configuration:" <+>
    (pTypeToDoc Unicode m)
