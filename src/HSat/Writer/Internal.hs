{-|
Module      : HSat.Writer.Internal
Description : Generic 'Writer' data types
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports generic data tyupes and functions for creating 'Comment's
-}

module HSat.Writer.Internal (
  -- * Data Type
  Orientation(..),
  Comment(..),
  -- * Constructors
  mkComment,
  -- * Evaluate
  runComment
  ) where

import Data.Text as T 
import HSat.Printer hiding ((<>))
import Data.Monoid

{-|
A Binary data type that describes whetehr a comment will be above or below the
data of interest
-}
data Orientation =
  Above |
  Below
  deriving (Eq)

instance Show Orientation where
  showsPrec = show'

instance Printer Orientation where
  compact Below = text "BLW"
  compact Above = text "ABV"
  noUnicode Above = text "Above"
  noUnicode Below = text "Below"
  unicode = noUnicode
  
{-|
'Comment' has two types; the Orientation of the text, and the 'Text' itself
-}
data Comment = Comment {
  -- | The orientation of the comment
  orientation :: Orientation,
  -- | The Text of the comment
  commentText :: Text
  } deriving (Eq)

instance Show Comment where
  showsPrec = show'

instance Printer Comment where
  compact (Comment orient commText) = compact orient <+> compact commText
  noUnicode  = compact 
  unicode  = compact 

instance Printer Text where
  compact t = text (unpack t)
  noUnicode = compact
  unicode = compact

{-|
Makes a commoent, but also makes sure that no newline characters are included in the comments
text by filtering them out
-}
mkComment :: Orientation -> Text -> Comment
mkComment o t = Comment o (T.filter f t)
  where
    f :: Char -> Bool
    f c = c/='\n' && c/='\r'

{-|
Returns a tuple of litsts of text; the first is the text above, the second below the data
item of interest
-}
runComment :: [Comment] -> ([Text],[Text])
runComment comments = runComment' comments ([],[])
  where
    runComment' :: [Comment] -> ([Text],[Text]) -> ([Text],[Text])
    runComment' [] result = result
    runComment' (x:xs) (l,r) =
      case orientation x of
        Above -> runComment' xs (l ++ [c <> commentText x],r)
        Below -> runComment' xs (l,r ++ [c <> commentText x])
    c :: Text
    c = pack "c "
