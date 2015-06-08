{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HSat.Printer
Description : Module for Pretty Printing
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

The Printer module exports functions for pretty printing HSat-specific data structures
-}

module HSat.Printer (
  -- * Exported Modules
  module Text.PrettyPrint.ANSI.Leijen,
  -- * Printer
  Printer(..),
  -- * Printer Type
  PrinterType(..),
  -- * Rendering
  pTypeToDoc, -- :: (Printer a) => PrinterType -> a -> Doc
  -- * Helper Functions
  show',      -- :: Printer a => Int -> a -> Doc
  errorDoc,   -- :: PrinterType -> Doc -> Doc
  toDoc       -- :: Show a => a -> Doc
  ) where

import Text.PrettyPrint.ANSI.Leijen

{-|
The 'Printer' class is used to render an arbitrary type to a pretty-string.

There are three modes associated with the 'Printer' type
-}
class Printer a where
  -- | Renders a type in a compact way; for example, a 'Bool' may be rendered
  -- | as T or F rather than 'True' or 'False'
  compact   :: a -> Doc
  -- | A verbose method of rendering output. Unicode characters are not used
  noUnicode :: a -> Doc
  -- | A verbose method of rendering output. Unicode characters and colour are used
  unicode   :: a -> Doc

{-|
A data type that enumerates the different Printer types. When used in conjunction with
'pTypeToDoc' will automatically render the output in the specified 'PrinterType'
-}
data PrinterType =
  Compact   | -- ^ Compact output specified
  NoUnicode | -- ^ No Unicode output specified
  Unicode     -- ^ Unicode output specified
  deriving (Eq,Show,Enum)

instance Printer PrinterType where
  compact    = print'
  noUnicode  = print'
  unicode    = print'

--Creates simple 'Doc' type for PrinterType data types
print'           :: PrinterType -> Doc
print' Compact   = "Compact"
print' Unicode   = "Unicode"
print' NoUnicode = "NoUnicode"

{-|
Takes a 'Printer' and a 'PrinterType' and renders the data type in the specified
rendering function
-}
pTypeToDoc           :: Printer a => PrinterType -> a -> Doc
pTypeToDoc Compact   = compact
pTypeToDoc NoUnicode = noUnicode
pTypeToDoc Unicode   = unicode

{-|
Can be used to quickly write a 'Show' instance for a type that already has a 'Printer'
instance.

Its intended use is

@
instance Show X where
  showsPrec = show'
@
-}
show' :: Printer a => Int -> a -> ShowS
show' i a = displayS $ renderPretty 0.4 i $ compact a

{-|
Takes a type with a 'Show' instance and produces a 'Doc'
-}
toDoc :: (Show a) => a -> Doc
toDoc = text . show

{-|
Takes a 'PrinterType' and a 'Doc' describing an error, and produces
a bespoke error message.

It should be used with all error messages within HSat
-}
errorDoc :: PrinterType -> Doc -> Doc
errorDoc Compact reason   = "ERR"   <> colon <+> reason
errorDoc NoUnicode reason = "Error" <> colon <+> reason
errorDoc Unicode reason   = red $ errorDoc NoUnicode reason

instance Printer Bool where
  compact   True  = "T"
  compact   False = "F"
  noUnicode True  = "True"
  noUnicode False = "False"
  unicode   True  = green "True"
  unicode   False = red "False"

instance Printer Word where
  compact   = toDoc
  noUnicode = toDoc
  unicode   = toDoc

instance Printer Double where
  compact   = toDoc
  noUnicode = toDoc
  unicode   = unicode
