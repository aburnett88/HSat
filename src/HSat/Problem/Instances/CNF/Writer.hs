{-# LANGUAGE
   OverloadedStrings,
   RecordWildCards
   #-}

{-|
Module      : HSat.Writer.CNF
Description : Used when adding comments to CNF files
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

When you wisht o add comments to a CNF file, this module can be used
-}

module HSat.Problem.Instances.CNF.Writer (
  toText
  ) where

import Data.Text as T hiding (map)
import HSat.Problem.Instances.CNF.Internal
import qualified Data.Vector as V
import HSat.Problem.Instances.Common
import Data.Monoid
import Data.Vector (Vector)
import HSat.Problem.Instances.Common.Clause.Internal (Clause(..))

{-|
Get an underlying CNF from a 'CNFWriter'

-}
toText :: CNF -> Text
toText CNF{..} =
  preamble  <>
  (mconcat $ makeClauses $ getVectClause getClauses)
  where
    preamble :: Text
    preamble = "p cnf " <> (pack $ show getMaxVar) <> " " <> (pack $ show getClauseNumb) <> "\n"
    makeClauses :: Vector Clause -> [Text]
    makeClauses = map (flip (<>) "\n") . map makeClause . V.toList
    makeClause :: Clause -> Text
    makeClause Clause{..} = (mconcat $ V.toList . V.map makeLit $ getVectLiteral) <> "0"
    makeLit :: Literal -> Text
    makeLit Literal{..} = past <> (pack . show . getWord $ getVariable) <> " "
      where
        past = if isPos getSign then empty else "-"
                            
