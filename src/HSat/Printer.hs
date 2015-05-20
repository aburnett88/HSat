{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HSat.Printer
Description : The Printer type used for showing output
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Within HSat, this Printer type is designed to be used to pretty print internal
data structuresto text for output.

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

import Text.PrettyPrint.ANSI.Leijen

{-|
The 'Printer' class is used to convert arbitrary types to 'Doc' types.

The class comes with four methods, none of which have to be defined, however
for more quality output, the user may decide to do so.
-}
class Printer a where
  -- | Compact should attempt to save space wherever possible. For example, a
  -- |'Bool' values may be T and F rather than 'True' and 'False' in a 'Show'
  -- | instance.
  compact   :: a -> Doc
  -- | Verbose output for where Unicode and colour is not available. However,
 -- | this is not enforced by the type system
  noUnicode :: a -> Doc
  -- | Coloured output is for a Unix system that allows colour and unicode
  -- |symbols to be used
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
Turns any 'Printer' type to a 'Doc' type using the appropriate 'Printing'
function
-}
pTypeToDoc           :: Printer a => PrinterType -> a -> Doc
pTypeToDoc Compact   = compact
pTypeToDoc NoUnicode = noUnicode
pTypeToDoc Unicode   = unicode

{-|
Takes an integer and a Printable type and creates a ShowS function.

Can be dropped in for a Show instance when a compact instance has been
created for a data type
-}
show' :: Printer a => Int -> a -> ShowS
show' i a = displayS $ renderPretty 0.4 i $ compact a

{-|
Turns a 'Word' into a 'Doc'
-}
word :: Word -> Doc
word = text . show

toDoc :: (Show a) => a -> Doc
toDoc = text . show

{-|
Takes a 'Doc' describing an error, and the 'PrinterType' and creates
a bespoke error message.

Used to standardise error messages across the HSat modules
-}
errorDoc :: PrinterType -> Doc -> Doc
errorDoc Compact reason   = "ERR"   <> colon <+> reason
errorDoc NoUnicode reason = "Error" <> colon <+> reason
errorDoc Unicode reason   = red $ errorDoc NoUnicode reason

instance Printer Bool where
  compact True    = "T"
  compact False   = "F"
  noUnicode True  = "True"
  noUnicode False = "False"
  unicode True    = green "True"
  unicode False   = red "False"

instance Printer Word where
  compact w = word w
  noUnicode w = word w
  unicode w = word w

instance Printer Double where
  compact d = toDoc d
  noUnicode d = toDoc d
  unicode d = unicode d
