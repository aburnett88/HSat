module HSat.Writer.Internal (
  Orientation(..),
  Comment(..),
  mkComment,
  runComment
  ) where

import Data.Text as T 
import HSat.Printer hiding ((<>))
import Data.Monoid

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
  unicode a = noUnicode a
  

data Comment = Comment {
  orientation :: Orientation,
  commentText :: Text
  } deriving (Eq)

instance Show Comment where
  showsPrec = show'

instance Printer Comment where
  compact (Comment orient commText) = compact orient <+> compact commText
  noUnicode comment = compact comment
  unicode comment = compact comment

instance Printer Text where
  compact t = text (unpack t)
  noUnicode t = compact t
  unicode t = compact t

mkComment :: Orientation -> Text -> Comment
mkComment o t = Comment o (T.filter f t)
  where
    f :: Char -> Bool
    f c = (c/='\n' && c/='\r')

runComment :: [Comment] -> ([Text],[Text])
runComment comments = runComment' comments ([],[])
  where
    runComment' :: [Comment] -> ([Text],[Text]) -> ([Text],[Text])
    runComment' [] result = result
    runComment' (x:xs) (l,r) =
      case orientation x of
        Above -> runComment' xs (l ++ [c <> (commentText x)],r)
        Below -> runComment' xs (l,r ++ [c <> (commentText x)])
    c :: Text
    c = (pack "c ")
