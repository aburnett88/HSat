module HSat.Writer.Internal (
  Orientation(..),
  Comment(..),
  mkComment,
  runComment
  ) where

import Data.Text
import HSat.Printer

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
  compact (Comment orientation commentText) = compact orientation <+> compact commentText
  noUnicode comment = compact comment
  unicode comment = compact comment

instance Printer Text where
  compact t = text (unpack t)
  noUnicode t = compact t
  unicode t = compact t

mkComment :: Orientation -> Text -> Comment
mkComment = Comment

runComment :: [Comment] -> ([Text],[Text])
runComment xs = runComment' xs ([],[])
  where
    runComment' :: [Comment] -> ([Text],[Text]) -> ([Text],[Text])
    runComment' [] result = result
    runComment' (x:xs) (l,r) =
      case orientation x of
        Above -> runComment' xs (l ++ [commentText x],r)
        Below -> runComment' xs (l,r ++ [commentText x])
