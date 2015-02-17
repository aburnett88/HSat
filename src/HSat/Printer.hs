{-|
Module      : HSat.Printer
Description : The Printer type used for showing output
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Within HSat, this Printer type is designed to be used to pretty print internal data
structuresto text for output.

Each type contains four modes of sorts, as described below.
-}

module HSat.Printer (
  -- Exported Modules
  module Text.PrettyPrint.ANSI.Leijen,
  -- * Printer Functions & Types
  Printer(..),
  pTypeToDoc,
  PrinterType(..),
  show',
  word,
  errorDoc
  ) where

import Data.Word
import Text.PrettyPrint.ANSI.Leijen

{-|
The 'Printer' class is used to convert arbitrary types to 'Doc' types.

The class comes with four methods, none of which have to be defined, however for more
quality output, the user may decide to do so.
-}
class Printer a where
  -- | Compact should attempt to save space wherever possible. For example, a 'Bool'
  -- values may be T and F rather than 'True' and 'False' in a 'Show' instance.
  compact   :: a -> Doc
  -- | Verbose output for where Unicode and colour is not available. However, this is
  -- not enforced by the type system
  noUnicode :: a -> Doc
  -- | Coloured output is for a Unix system that allows colour and unicode symbols to
  -- be used
  unicode   :: a -> Doc

{-|
This data type is used to denote which type of 'Printer' function ot use
-}
data PrinterType =
  Compact   | -- ^ Compact output specified
  NoUnicode | -- ^ No unicode output specified
  Unicode     -- ^ Unicode output specified
  deriving (Eq,Show,Enum)

{-|
Turns any 'Printer' type to a 'Doc' type using the appropriate 'Printing' function
-}
pTypeToDoc           :: Printer a => PrinterType -> a -> Doc
pTypeToDoc Compact   = compact
pTypeToDoc NoUnicode = noUnicode
pTypeToDoc Unicode   = unicode

show' :: Printer a => Int -> a -> ShowS
show' i a = displayS (renderPretty 0.4 i (compact a))

word :: Word -> Doc
word = text . show

errorDoc :: PrinterType -> Doc -> Doc
errorDoc Compact reason   = (text "ERR")   <> colon <+> reason
errorDoc NoUnicode reason = (text "Error") <> colon <+> reason
errorDoc Unicode reason   = red $ errorDoc NoUnicode reason
