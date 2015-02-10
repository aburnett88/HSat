module HSat.Writer.Internal (
  Orientation(..),
  Comment(..),
  mkComment,
  runComment
  ) where

import Data.Text

data Orientation =
  Above |
  Below
  deriving (Eq,Show)

data Comment = Comment {
  orientation :: Orientation,
  commentText :: Text
  } deriving (Eq,Show)

mkComment :: Orientation -> Text -> Comment
mkComment o t = Comment o t

runComment :: [Comment] -> ([Text],[Text])
runComment xs = runComment' xs ([],[])
  where
    runComment' :: [Comment] -> ([Text],[Text]) -> ([Text],[Text])
    runComment' [] result = result
    runComment' (x:xs) (l,r) =
      case orientation x of
        Above -> runComment' xs (l ++ [commentText x],r)
        Below -> runComment' xs (l,r ++ [commentText x])
