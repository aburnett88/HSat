{-|
Module      : HSat.Utils.Printer
Description : Describes the Printer type
Copyright   : (c) Andrew Burnett, 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Within HSat, this printer type is used extensively to print raw
data, compact data and verbose data both with and without
colouring and Unicode symbols. 
-}

module HSat.Utils.Printer (
  -- * Printer
  Printer(..),
  pTypeToDoc,
  PrinterType(..),
  -- * Document Exports
  module Text.PrettyPrint.ANSI.Leijen
  ) where

import Text.PrettyPrint.ANSI.Leijen

{- |
The pretty printer class used to print variables in HSat.

It should be noted that there is no minimal definition by default, nor is there any kind of attempts to create type
safe functions for these types. It is simply up to the user
to experiment. 
-}
class (Show a) => Printer a where
  -- | Raw output should be similarr or the same as a show
  -- class. It is best used for debugging purposes. It's
  -- default signature simply packs a show instance into a Doc
  -- type
  raw :: a -> Doc
  raw = text . show
  -- | Compact output should be as compact as possible. It should be easilly
  -- readable for someone who understands the program.
  compact :: a -> Doc
  compact = raw
  -- | Verbose output for systems where colour and Unicode symbols are not
  -- provided. 
  noAnsi :: a -> Doc
  noAnsi = raw
  -- | Coloured output is for systems that allow use of Unicode symbols and
  -- colourised output. It can use these colours and symbols to help users
  -- visualise data in a concise way. 
  coloured :: a -> Doc
  coloured = raw

{- |
A type that corresponds to which type of of document to produce. Production can
be called using the denotePrinter function
-}
data PrinterType =
  Raw     | -- ^ Raw output. Default is the show instance
  Compact | -- ^ Compact output
  NoAnsi  | -- ^ Output where ANSI support is limited. Should contain no unicode, or colouring
  Coloured  -- ^ Coloured output. Unicode symbols enabled. 
  deriving (Eq,Show,Read)

{- |
pTypeToDoc prints a Printer type, when given a type denoting which printer to use. 
-}

pTypeToDoc          :: Printer a => PrinterType -> a -> Doc
pTypeToDoc Raw      = raw
pTypeToDoc Compact  = compact
pTypeToDoc NoAnsi   = noAnsi
pTypeToDoc Coloured = coloured
