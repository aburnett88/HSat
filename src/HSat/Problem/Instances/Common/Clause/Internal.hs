{-|
Module      : HSat.Problem.Instances.Common.Clause.Internal
Description : The Clause data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The module defines the internal structure of a 'Clause'.

A 'Clause' is simply a collection of 'Literal's
-}

module HSat.Problem.Instances.Common.Clause.Internal (
  Clause(..),
  printClauseWithContext -- :: String -> Word -> (Literal -> Doc) -> Clause -> Doc
  ) where

import           Data.Vector                            (Vector)
import qualified Data.Vector                            as V
import           HSat.Printer
import           HSat.Problem.Instances.Common.Literal
import           HSat.Problem.Instances.Common.Variable (getWord)

{-|
A 'Clause' describes a finite number of 'Literal's to be used as a generic
data structure for describing Boolean formulae.

Internally a 'Clause' is represented as a 'Vector' of 'Literal's and a cached
'Word' of its size. 
-}
data Clause = Clause {
  -- | The underlying 'Vector' of 'Literal's that is represented
  getVectLiteral   :: Vector Literal,
  -- | The cached size of the 'Clause' in 'Word' form
  getSizeClause :: Word
  } deriving (Eq)

instance Show Clause where
  showsPrec = show'

instance Printer Clause where
  compact   = printLit compact
  noUnicode = printLit noUnicode
  unicode   = printLit unicode

printLit                 :: (Literal -> Doc) -> Clause -> Doc
printLit function clause =
  lbracket                                                                   <>
  hsep (punctuate comma (map function . V.toList . getVectLiteral $ clause)) <>
  rbracket

{-|
Takes a 'String' to separate each 'Literal', a 'Word' denoting the maximum 'Variable'
in the whole context, a function that turns a 'Literal' to a 'Doc' and the 'Clause'
itself, and separates each literal by the 'String', making sure that any row will line
up
-}
--Untested as a printing function
printClauseWithContext                                  :: String -> Word -> (Literal -> Doc) ->
                                                           Clause -> Doc
printClauseWithContext sepClause maxVar function clause =
  lparen                                     <>
  hsep (punctuate (text sepClause) literals) <>
  rparen
  where
    literals        :: [Doc]
    literals        = V.toList . V.map padding $ getVectLiteral clause
    padding         :: Literal -> Doc
    padding literal =
      let maxVarLen  = length $ show maxVar
          varLen     = length . show . getWord $ getVariable literal
          difference = maxVarLen - varLen
      in text (replicate difference ' ') <> function literal
