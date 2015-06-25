{-# LANGUAGE
    OverloadedStrings
    #-}

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

import HSat.Make.Config.Class
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
  
instance Printer Source where
  compact   = printerSource Compact
  noUnicode = printerSource NoUnicode
  unicode   = printerSource Unicode

printerSource                       :: PrinterType -> Source -> Doc
printerSource pType StaticSource    = static
  where
    static  :: Doc
    static  = case pType of
      Compact    -> "STATIC"
      NoUnicode  -> static'
      Unicode    -> green static'
    static' :: Doc
    static' = "Static Source"
printerSource pType (FileSource fp) =
  preamble <+> fileSource
  where
    preamble   :: Doc
    preamble   = (
      case pType of
       Compact -> "FILE"
       _       -> "FilePath"
      ) <> colon
    fileSource :: Doc
    fileSource =
      case pType of
       Compact   -> case compare lenSize (length fp) of
         GT -> text (take (lenSize - length dots) fp) <> text dots
         _  -> text fp
       NoUnicode -> text fp
       Unicode   -> yellow $ text fp
    lenSize    :: Int
    lenSize    = 20
    dots       :: String
    dots       = "..."
printerSource pType (MakeConfiguration m) =
  preamble <> makeDoc
  where
    preamble :: Doc
    preamble = (
      case pType of
        Compact -> "MAKE"
        _ -> "Make") <> colon
    makeDoc  :: Doc
    makeDoc  = pTypeToDoc pType m
